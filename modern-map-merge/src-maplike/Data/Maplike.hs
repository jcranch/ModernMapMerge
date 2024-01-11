{-# LANGUAGE
      FlexibleInstances,
      FunctionalDependencies,
      MultiParamTypeClasses,
      TupleSections,
      TypeOperators
  #-}

module Data.Maplike where

-- TODO Replicate more standard map functionality

import Prelude hiding (lookup, null)

import Data.Bifunctor
import Data.Foldable.WithIndex (FoldableWithIndex(..))
import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))
import Data.Functor.WithIndex (FunctorWithIndex(..))
import Data.IntMap (IntMap)
import qualified Data.IntMap as I
import qualified Data.IntMap.Merge as I
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Map.Merge as M
import Data.Maybe (isNothing)
import Data.Traversable.WithIndex (TraversableWithIndex(..))

import Witherable (Filterable(..),
                   Witherable(..),
                   FilterableWithIndex(..),
                   WitherableWithIndex(..))

import qualified Data.Map.Extra as ME
import qualified Data.IntMap.Extra as IE
import Data.MergeTactics        (WhenMissing,
                                 missingKey,
                                 dropMissing,
                                 preserveMissing,
                                 reindexMissing,
                                 WhenMatched(..),
                                 preserveLeftMatched,
                                 zipWithMatched,
                                 zipWithAMatched,
                                 reindexMatched)


class (FilterableWithIndex k m, WitherableWithIndex k m) => Maplike k m | m -> k where

  empty :: m v

  null :: m v -> Bool

  singleton :: k -> v -> m v

  lookup :: k -> m v -> Maybe v
  lookup k = getConst . alterF Const k

  alterF :: (Functor f)
         => (Maybe v -> f (Maybe v))
         -> k
         -> m v
         -> f (m v)

  alter :: (Maybe v -> Maybe v) -> k -> m v -> m v
  alter f k = runIdentity . alterF (Identity . f) k

  insert :: k -> v -> m v -> m v
  insert k v = alter (const $ Just v) k

  minViewWithKey :: m v -> Maybe ((k, v), m v)
  minViewWithKey = alterMinWithKeyF (\k v -> ((k, v), Nothing))

  maxViewWithKey :: m v -> Maybe ((k, v), m v)
  maxViewWithKey = alterMaxWithKeyF (\k v -> ((k, v), Nothing))

  minView :: m v -> Maybe (v, m v)
  minView = alterMinF (,Nothing)

  maxView :: m v -> Maybe (v, m v)
  maxView = alterMaxF (,Nothing)

  alterMinF :: Functor f => (v -> f (Maybe v)) -> m v -> Maybe (f (m v))
  alterMinF = alterMinWithKeyF . const

  alterMaxF :: Functor f => (v -> f (Maybe v)) -> m v -> Maybe (f (m v))
  alterMaxF = alterMaxWithKeyF . const

  -- | A safer, more general version of `Data.Map.updateMinWithKey`:
  -- change the minimum key, but if the map is empty, return `Nothing`
  alterMinWithKeyF :: Functor f => (k -> v -> f (Maybe v)) -> m v -> Maybe (f (m v))

  -- | A safer, more general version of `Data.Map.updateMaxWithKey`:
  -- change the maximum key, but if the map is empty, return `Nothing`
  alterMaxWithKeyF :: Functor f => (k -> v -> f (Maybe v)) -> m v -> Maybe (f (m v))

  -- Merge two data structures
  -- Some data structures can implement unindexed merging rather more
  -- efficiently. This is particularly true of tries, where accessing
  -- the keys requires assembling a list.
  merge :: WhenMissing Identity () a c -- ^ What to do with keys in @m1@ but not @m2@
        -> WhenMissing Identity () b c -- ^ What to do with keys in @m2@ but not @m1@
        -> WhenMatched Identity () a b c -- ^ What to do with keys in both @m1@ and @m2@
        -> m a -- ^ Map @m1@
        -> m b -- ^ Map @m2@
        -> m c
  merge onL onR onB u v = runIdentity $ mergeA onL onR onB u v

  -- Merge, valued in an applicative functor
  mergeA :: (Applicative f)
         => WhenMissing f () a c -- ^ What to do with keys in @m1@ but not @m2@
         -> WhenMissing f () b c -- ^ What to do with keys in @m2@ but not @m1@
         -> WhenMatched f () a b c -- ^ What to do with keys in both @m1@ and @m2@
         -> m a -- ^ Map @m1@
         -> m b -- ^ Map @m2@
         -> f (m c)
  mergeA onL onR onB = let
    onL' = reindexMissing (const ()) onL
    onR' = reindexMissing (const ()) onR
    onB' = reindexMatched (const ()) onB
    in imergeA onL' onR' onB'

  -- Merge, using indices
  imerge :: WhenMissing Identity k a c -- ^ What to do with keys in @m1@ but not @m2@
         -> WhenMissing Identity k b c -- ^ What to do with keys in @m2@ but not @m1@
         -> WhenMatched Identity k a b c -- ^ What to do with keys in both @m1@ and @m2@
         -> m a -- ^ Map @m1@
         -> m b -- ^ Map @m2@
         -> m c
  imerge onL onR onB u v = runIdentity $ imergeA onL onR onB u v

  -- Merge, using indices, valued in an applicative functor
  imergeA :: (Applicative f)
          => WhenMissing f k a c -- ^ What to do with keys in @m1@ but not @m2@
          -> WhenMissing f k b c -- ^ What to do with keys in @m2@ but not @m1@
          -> WhenMatched f k a b c -- ^ What to do with keys in both @m1@ and @m2@
          -> m a -- ^ Map @m1@
          -> m b -- ^ Map @m2@
          -> f (m c)

-- | A helper function that occurs all over the place
nonNull :: Maplike k m => m v -> Maybe (m v)
nonNull m
  | null m    = Nothing
  | otherwise = Just m

instance Maplike () Maybe where
  empty = Nothing
  null = isNothing
  singleton _ = Just
  alterF a _ m = a m
  minViewWithKey Nothing = Nothing
  minViewWithKey (Just x) = Just (((),x), Nothing)
  maxViewWithKey Nothing = Nothing
  maxViewWithKey (Just x) = Just (((),x), Nothing)
  alterMinWithKeyF _ Nothing = Nothing
  alterMinWithKeyF f (Just x) = Just (f () x)
  alterMaxWithKeyF _ Nothing = Nothing
  alterMaxWithKeyF f (Just x) = Just (f () x)
  imergeA _ _ _ Nothing Nothing = pure Nothing
  imergeA l _ _ (Just x) Nothing = missingKey l () x
  imergeA _ r _ Nothing (Just y) = missingKey r () y
  imergeA _ _ b (Just x) (Just y) = matchedKey b () x y

instance Ord k => Maplike k (Map k) where
  empty = M.empty
  null = M.null
  singleton = M.singleton
  alterF = M.alterF
  minView = M.minView
  maxView = M.maxView
  minViewWithKey = M.minViewWithKey
  maxViewWithKey = M.maxViewWithKey
  alterMinWithKeyF = ME.alterMinWithKeyF
  alterMaxWithKeyF = ME.alterMaxWithKeyF
  imergeA = M.mergeA

instance Maplike Int IntMap where
  empty = I.empty
  null = I.null
  singleton = I.singleton
  alterF = I.alterF
  minView = I.minView
  maxView = I.maxView
  minViewWithKey = I.minViewWithKey
  maxViewWithKey = I.maxViewWithKey
  alterMinWithKeyF = IE.alterMinWithKeyF
  alterMaxWithKeyF = IE.alterMaxWithKeyF
  imergeA = I.mergeA


-- | Union, preferring left
union :: Maplike k m => m v -> m v -> m v
union = merge preserveMissing preserveMissing preserveLeftMatched

-- | Union, with combining function
unionWith :: Maplike k m => (v -> v -> v) -> m v -> m v -> m v
unionWith f = merge preserveMissing preserveMissing (zipWithMatched $ const f)

-- | Intersection, preferring left
intersection :: Maplike k m => m v -> m v -> m v
intersection = merge dropMissing dropMissing preserveLeftMatched

-- | Intersection, with combining function
intersectionWith :: Maplike k m => (v -> v -> v) -> m v -> m v -> m v
intersectionWith f = merge dropMissing dropMissing (zipWithMatched $ const f)

-- | Combine matching pairs
pairBy :: (Maplike k m, Monoid a) => (u -> v -> a) -> m u -> m v -> a
pairBy f u v = let
  m = mergeA dropMissing dropMissing (zipWithAMatched (\_ x y -> Const $ f x y))
  in getConst $ m u v

-- | Combine matching pairs
ipairBy :: (Maplike k m, Monoid a) => (k -> u -> v -> a) -> m u -> m v -> a
ipairBy f u v = let
  m = imergeA dropMissing dropMissing (zipWithAMatched (\i x y -> Const $ f i x y))
  in getConst $ m u v


data OnMaybe k m v = OnMaybe {
  onNothing :: Maybe v,
  onJust :: m v
} deriving (Eq, Ord, Read, Show)

instance Functor m => Functor (OnMaybe k m) where
  fmap f (OnMaybe x u) = OnMaybe (fmap f x) (fmap f u)

instance FunctorWithIndex k m => FunctorWithIndex (Maybe k) (OnMaybe k m) where
  imap f (OnMaybe x u) = OnMaybe (fmap (f Nothing) x) (imap (f . Just) u)

instance Foldable m => Foldable (OnMaybe k m) where
  foldr f a (OnMaybe x u) = foldr f (foldr f a u) x

instance FoldableWithIndex k m => FoldableWithIndex (Maybe k) (OnMaybe k m) where
  ifoldMap f (OnMaybe x u) = foldMap (f Nothing) x <> ifoldMap (f . Just) u
  ifoldr f a (OnMaybe x u) = foldr (f Nothing) (ifoldr (f . Just) a u) x

instance Traversable m => Traversable (OnMaybe k m) where
  traverse f (OnMaybe x u) = liftA2 OnMaybe (traverse f x) (traverse f u)

instance TraversableWithIndex k m => TraversableWithIndex (Maybe k) (OnMaybe k m) where
  itraverse f (OnMaybe x u) = liftA2 OnMaybe (traverse (f Nothing) x) (itraverse (f . Just) u)

instance Filterable m => Filterable (OnMaybe k m) where
  mapMaybe f (OnMaybe x u) = OnMaybe (f =<< x) (mapMaybe f u)

instance FilterableWithIndex k m => FilterableWithIndex (Maybe k) (OnMaybe k m) where
  imapMaybe f (OnMaybe x u) = OnMaybe (mapMaybe (f Nothing) x) (imapMaybe (f . Just) u)

instance Witherable m => Witherable (OnMaybe k m) where
  wither f (OnMaybe x u) = liftA2 OnMaybe (wither f x) (wither f u)

instance WitherableWithIndex k m => WitherableWithIndex (Maybe k) (OnMaybe k m) where
  iwither f (OnMaybe x u) = liftA2 OnMaybe (wither (f Nothing) x) (iwither (f . Just) u)

instance Maplike k m => Maplike (Maybe k) (OnMaybe k m) where

  empty = OnMaybe Nothing empty

  null (OnMaybe x u) = null x && null u

  singleton Nothing  x = OnMaybe (Just x) empty
  singleton (Just a) x = OnMaybe Nothing $ singleton a x

  lookup Nothing  (OnMaybe x _) = x
  lookup (Just z) (OnMaybe _ u) = lookup z u

  alterF f Nothing  (OnMaybe x u) = flip OnMaybe u <$> f x
  alterF f (Just a) (OnMaybe x u) = OnMaybe x <$> alterF f a u

  alter f Nothing  (OnMaybe x u) = flip OnMaybe u $ f x
  alter f (Just a) (OnMaybe x u) = OnMaybe x $ alter f a u

  insert Nothing y (OnMaybe _ u) = OnMaybe (Just y) u
  insert (Just a) y (OnMaybe x u) = OnMaybe x $ insert a y u

  minViewWithKey (OnMaybe x u) = case x of
    Just y  -> Just ((Nothing, y), OnMaybe Nothing u)
    Nothing -> fmap (bimap (first Just) (OnMaybe x)) $ minViewWithKey u

  maxViewWithKey (OnMaybe x u) = case maxViewWithKey u of
    Just ((k, y), u') -> Just ((Just k, y), OnMaybe x u')
    Nothing -> case x of
      Just y  -> Just ((Nothing, y), OnMaybe Nothing u)
      Nothing -> Nothing

  alterMinWithKeyF f (OnMaybe x u) = case x of
    Just y  -> Just . fmap (flip OnMaybe u) $ f Nothing y
    Nothing -> fmap (fmap (OnMaybe x)) $ alterMinWithKeyF (f . Just) u

  alterMaxWithKeyF f (OnMaybe x u) = case alterMaxWithKeyF (f . Just) u of
    Just u' -> Just (OnMaybe x <$> u')
    Nothing -> case x of
      Just y  -> Just (flip OnMaybe u <$> f Nothing y)
      Nothing -> Nothing

  imergeA l r b (OnMaybe x u) (OnMaybe y v) = let
    z = imergeA (reindexMissing (const Nothing) l) (reindexMissing (const Nothing) r) (reindexMatched (const Nothing) b) x y
    w = imergeA (reindexMissing Just l) (reindexMissing Just r) (reindexMatched Just b) u v
    in liftA2 OnMaybe z w


data OnEither k l m n v = OnEither {
  onLeft  :: m v,
  onRight :: n v
} deriving (Eq, Ord, Read, Show)

instance (Functor m, Functor n) => Functor (OnEither k l m n) where
  fmap f (OnEither m n) = OnEither (fmap f m) (fmap f n)

instance (FunctorWithIndex k m, FunctorWithIndex l n) => FunctorWithIndex (Either k l) (OnEither k l m n) where
  imap f (OnEither m n) = OnEither (imap (f . Left) m) (imap (f . Right) n)

instance (Foldable m, Foldable n) => Foldable (OnEither k l m n) where
  foldMap f (OnEither m n) = foldMap f m <> foldMap f n
  foldr f z (OnEither m n) = foldr f (foldr f z n) m

instance (FoldableWithIndex k m, FoldableWithIndex l n) => FoldableWithIndex (Either k l) (OnEither k l m n) where
  ifoldMap f (OnEither m n) = ifoldMap (f . Left) m <> ifoldMap (f . Right) n
  ifoldr f z (OnEither m n) = ifoldr (f . Left) (ifoldr (f . Right) z n) m

instance (Traversable m, Traversable n) => Traversable (OnEither k l m n) where
  traverse f (OnEither m n) = liftA2 OnEither (traverse f m) (traverse f n)

instance (TraversableWithIndex k m, TraversableWithIndex l n) => TraversableWithIndex (Either k l) (OnEither k l m n) where
  itraverse f (OnEither m n) = liftA2 OnEither (itraverse (f . Left) m) (itraverse (f . Right) n)

instance (Filterable m, Filterable n) => Filterable (OnEither k l m n) where
  mapMaybe f (OnEither m n) = OnEither (mapMaybe f m) (mapMaybe f n)

instance (FilterableWithIndex k m, FilterableWithIndex l n) => FilterableWithIndex (Either k l) (OnEither k l m n) where
  imapMaybe f (OnEither m n) = OnEither (imapMaybe (f . Left) m) (imapMaybe (f . Right) n)

instance (Witherable m, Witherable n) => Witherable (OnEither k l m n) where
  wither f (OnEither m n) = liftA2 OnEither (wither f m) (wither f n)

instance (WitherableWithIndex k m, WitherableWithIndex l n) => WitherableWithIndex (Either k l) (OnEither k l m n) where
  iwither f (OnEither m n) = liftA2 OnEither (iwither (f . Left) m) (iwither (f . Right) n)

instance (Maplike k m, Maplike l n) => Maplike (Either k l) (OnEither k l m n) where



newtype OnPair k l m n v = OnPair {
  getPair :: m (n v)
} deriving (Eq, Ord, Read, Show)

instance (Functor m, Functor n) => Functor (OnPair k l m n) where
  fmap f (OnPair m) = OnPair $ fmap (fmap f) m

instance (FunctorWithIndex k m, FunctorWithIndex l n) => FunctorWithIndex (k,l) (OnPair k l m n) where
  imap f (OnPair m) = OnPair $ imap (\k -> imap (f . (k,))) m

instance (Foldable m, Foldable n) => Foldable (OnPair k l m n) where
  foldMap f (OnPair m) = foldMap (foldMap f) m
  foldr f z (OnPair m) = foldr (flip (foldr f)) z m

instance (FoldableWithIndex k m, FoldableWithIndex l n) => FoldableWithIndex (k,l) (OnPair k l m n) where
  ifoldMap f (OnPair m) = ifoldMap (\k -> ifoldMap (f . (k,))) m
  ifoldr f z (OnPair m) = ifoldr (\k -> flip (ifoldr (f . (k,)))) z m

instance (Traversable m, Traversable n) => Traversable (OnPair k l m n) where
  traverse f (OnPair m) = OnPair <$> traverse (traverse f) m

instance (TraversableWithIndex k m, TraversableWithIndex l n) => TraversableWithIndex (k,l) (OnPair k l m n) where
  itraverse f (OnPair m) = OnPair <$> itraverse (\k -> itraverse (f . (k,))) m

instance (Filterable m, Maplike l n) => Filterable (OnPair k l m n) where
  mapMaybe f (OnPair m) = OnPair $ mapMaybe (nonNull . mapMaybe f) m

instance (FilterableWithIndex k m, Maplike l n) => FilterableWithIndex (k,l) (OnPair k l m n) where
  imapMaybe f (OnPair m) = OnPair $ imapMaybe (\k -> nonNull . imapMaybe (f . (k,))) m

instance (Witherable m, Maplike l n) => Witherable (OnPair k l m n) where
  wither f (OnPair m) = OnPair <$> wither (fmap nonNull . wither f) m

instance (WitherableWithIndex k m, Maplike l n) => WitherableWithIndex (k,l) (OnPair k l m n) where
  iwither f (OnPair m) = OnPair <$> iwither (\k -> fmap nonNull . iwither (f . (k,))) m

instance (Maplike k m, Maplike l n) => Maplike (k,l) (OnPair k l m n) where

  empty = OnPair empty

  null (OnPair m) = null m

  singleton (k,l) v = OnPair . singleton k $ singleton l v

  alterF f (k,l) (OnPair m) = let
    g Nothing  = let
      in fmap (fmap (singleton l)) $ f Nothing
    g (Just n) = nonNull <$> alterF f l n
    in OnPair <$> alterF g k m
