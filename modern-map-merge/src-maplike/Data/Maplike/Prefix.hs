{-# LANGUAGE
      CPP,
      DeriveFunctor,
      FlexibleContexts,
      FlexibleInstances,
      MultiParamTypeClasses,
      UndecidableInstances
  #-}

-- | Prefix maps: given a `Maplike` with keys @k@ we provide a Maplike
-- with keys @[k]@.
module Data.Maplike.Prefix where

import Prelude hiding (null)

#if MIN_VERSION_base(4,18,0)
#else
import Control.Applicative (liftA2)
#endif
import Data.Bifunctor (first)
import Data.Foldable.WithIndex
import Data.Functor.WithIndex
import Data.Traversable.WithIndex
import Witherable
import Data.MergeTactics (WhenMissing(..),
                          WhenMatched(..),
                          reindexMissing,
                          traverseMaybeMissing)

import Data.Maplike


data PrefixMap m v = PrefixMap {
  content :: Maybe v,
  children :: m (PrefixMap m v)
} deriving (Functor)


instance FunctorWithIndex k m => FunctorWithIndex [k] (PrefixMap m) where
  imap f (PrefixMap h t) = let
    g r = imap (f . (r:))
    in PrefixMap (f [] <$> h) (imap g t)

instance Foldable m => Foldable (PrefixMap m) where
  foldMap f = let
    g (PrefixMap a c) = let
      h Nothing = id
      h (Just x) = mappend $ f x
      in h a $ foldMap g c
    in g

instance FoldableWithIndex k m => FoldableWithIndex [k] (PrefixMap m) where
  ifoldMap f (PrefixMap a c) = let
    h Nothing = id
    h (Just x) = mappend (f [] x)
    r = let
      g x = ifoldMap (f . (x:))
      in ifoldMap g c
    in h a r

instance Traversable m => Traversable (PrefixMap m) where
  traverse f = let
    g (PrefixMap a c) = liftA2 PrefixMap (traverse f a) (traverse g c)
    in g

instance TraversableWithIndex k m => TraversableWithIndex [k] (PrefixMap m) where
  itraverse f (PrefixMap a c) = liftA2 PrefixMap (traverse (f []) a) (itraverse (\x -> itraverse (f . (x:))) c)

instance Maplike k m => Filterable (PrefixMap m) where
  mapMaybe f = let
    inner (PrefixMap a c) = PrefixMap {
      content = f =<< a,
      children = mapMaybe (nonNull . inner) c }
    in inner

instance Maplike k m => FilterableWithIndex [k] (PrefixMap m) where
  imapMaybe f = let
    inner p (PrefixMap a c) = PrefixMap {
      content = f (p []) =<< a,
      children = imapMaybe (\i -> nonNull . inner (p . (i:))) c }
    in inner id

instance Maplike k m => Witherable (PrefixMap m) where

instance Maplike k m => WitherableWithIndex [k] (PrefixMap m) where


instance Maplike k m => Maplike [k] (PrefixMap m) where

  empty = PrefixMap Nothing empty

  null (PrefixMap Nothing x) = null x
  null (PrefixMap (Just _) _) = False

  singleton [] x = PrefixMap (Just x) empty
  singleton (k:ks) x = PrefixMap Nothing . singleton k $ singleton ks x

  classify (PrefixMap a x) = let
    u = first (const []) (classify a)
    v = bindClassify (:) classify (classify x)
    in u <> v

  -- alterF :: (Functor f) => (Maybe v -> f (Maybe v)) -> [k] -> PrefixMap m v -> f (PrefixMap m v)
  alterF a = let

    -- j :: [k] -> Maybe (PrefixMap m v) -> f (Maybe (PrefixMap m v))
    j ks Nothing = fmap (singleton ks) <$> a Nothing
    j ks (Just m) = nonNull <$> go ks m

    -- go :: [k] -> PrefixMap m v -> f (PrefixMap m v)
    go [] (PrefixMap h t) = flip PrefixMap t <$> a h
    go (k:ks) (PrefixMap h t) = PrefixMap h <$> alterF (j ks) k t

    in go

  minViewWithKey (PrefixMap (Just x) m) = Just (([], x), PrefixMap Nothing m)
  minViewWithKey (PrefixMap Nothing m)  = let
    g _ Nothing           = error "minViewWithKey: unexpected empty part"
    g x (Just ((k,v),m')) = ((x:k,v), nonNull m')
    in fmap (PrefixMap Nothing) <$> alterMinWithKeyF (\x -> g x . minViewWithKey) m

  maxViewWithKey (PrefixMap a m) = let
    g _ Nothing           = error "maxViewWithKey: unexpected empty part"
    g x (Just ((k,v),m')) = ((x:k,v), nonNull m')
    h x = (([], x), PrefixMap Nothing m)
    in case alterMaxWithKeyF (\x -> g x . maxViewWithKey) m of
      Just u -> Just (PrefixMap a <$> u)
      Nothing -> h <$> a

  alterMinWithKeyF f (PrefixMap (Just x) m) = let
    h y = PrefixMap y m
    in Just (h <$> f [] x)
  alterMinWithKeyF f (PrefixMap Nothing m) = let
    g Nothing  = error "alterMinWithKeyF: unexpected empty part"
    g (Just u) = nonNull <$> u
    in fmap (PrefixMap Nothing) <$> alterMinWithKeyF (\x -> g . alterMinWithKeyF (f . (x:))) m

  alterMaxWithKeyF f (PrefixMap a m) = let
    g Nothing  = error "alterMaxWithKeyF: unexpected empty part"
    g (Just u) = nonNull <$> u
    h x = PrefixMap x m
    in case alterMaxWithKeyF (\x -> g . alterMaxWithKeyF (f . (x:))) m of
      Just u  -> Just (PrefixMap a <$> u)
      Nothing -> fmap h . f [] <$> a

  imergeA l r (WhenMatched b) = let

    onChildren p = let
      tl c = fmap nonNull . runWhenMissing (reindexMissing (p . (c:)) l)
      tr c = fmap nonNull . runWhenMissing (reindexMissing (p . (c:)) r)
      fb c m1 m2 = nonNull <$> go (p . (c:)) m1 m2
      in imergeA (traverseMaybeMissing tl) (traverseMaybeMissing tr) (WhenMatched fb)

    onContent p = let
      k = p []
      tl = reindexMissing (const k) l
      tr = reindexMissing (const k) r
      fb _ = b k
      in imergeA tl tr (WhenMatched fb)

    go p (PrefixMap h1 t1) (PrefixMap h2 t2) = liftA2 PrefixMap (onContent p h1 h2) (onChildren p t1 t2)

    in go id
