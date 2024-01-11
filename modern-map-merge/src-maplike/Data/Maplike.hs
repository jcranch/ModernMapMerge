{-# LANGUAGE
      FlexibleInstances,
      FunctionalDependencies,
      MultiParamTypeClasses,
      TupleSections,
      TypeOperators
  #-}

module Data.Maplike where

-- TODO Replicate more standard map functionality

import Prelude hiding (null)

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
      Just y -> Just ((Nothing, y), OnMaybe Nothing u)
      Nothing -> Nothing

  alterMinWithKeyF = _

data OnEither k l m n v =
  OnEitherL (m v) |
  OnEitherR (n v)
  deriving (Eq, Ord, Read, Show)

{-
  Functor,
  FunctorWithIndex (Either k l),
  Foldable,
  FoldableWithIndex (Either k l),
  Traversable,
  TraversableWithIndex (Either k l),
  Filterable,
  FilterableWithIndex (Either k l),
  Witherable,
  WitherableWithIndex (Either k l))

instance (Maplike k m, Maplike l n) => Maplike (Either k l) (OnEither k l m n) where
-}



newtype OnPair k l m n v = OnPair {
  getPair :: m (n v)
} deriving (Eq, Ord, Read, Show)
