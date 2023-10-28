{-# LANGUAGE
      FlexibleInstances,
      FunctionalDependencies,
      MultiParamTypeClasses
  #-}

module Data.Maplike where

import Prelude hiding (null)

import Data.IntMap (IntMap)
import qualified Data.IntMap as I
import qualified Data.IntMap.Merge as I
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Map.Merge as M
import Data.Maybe (isNothing)

import Data.Witherable.WithIndex
import Data.MergeTactics        (WhenMissing,
                                 missingKey,
                                 WhenMatched(..))


class WitherableWithIndex k m => Maplike k m | m -> k where

  empty :: m v

  null :: m v -> Bool

  singleton :: k -> v -> m v

  alterF :: (Functor f)
         => (Maybe v -> f (Maybe v))
         -> k
         -> m v
         -> f (m v)

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
  mergeA :: WhenMissing Identity f a c -- ^ What to do with keys in @m1@ but not @m2@
         -> WhenMissing Identity f b c -- ^ What to do with keys in @m2@ but not @m1@
         -> WhenMatched Identity f a b c -- ^ What to do with keys in both @m1@ and @m2@
         -> m a -- ^ Map @m1@
         -> m b -- ^ Map @m2@
         -> f (m c)
  mergeA onL onR onB = imergeA (reindexWhenMissing onL) (reindexWhenMissing onR) (reindexWhenMatched onB)

  -- Merge, using indices
  imerge :: WhenMissing Identity k a c -- ^ What to do with keys in @m1@ but not @m2@
         -> WhenMissing Identity k b c -- ^ What to do with keys in @m2@ but not @m1@
         -> WhenMatched Identity k a b c -- ^ What to do with keys in both @m1@ and @m2@
         -> m a -- ^ Map @m1@
         -> m b -- ^ Map @m2@
         -> m c
  imerge onL onR onB u v = runIdentity $ imergeA onL onR onB u v

  -- Merge, using indices, valued in an applicative functor
  imergeA :: Applicative f
          => WhenMissing f k a c -- ^ What to do with keys in @m1@ but not @m2@
          -> WhenMissing f k b c -- ^ What to do with keys in @m2@ but not @m1@
          -> WhenMatched f k a b c -- ^ What to do with keys in both @m1@ and @m2@
          -> m a -- ^ Map @m1@
          -> m b -- ^ Map @m2@
          -> f (m c)

nonNull :: Maplike k m => m v -> Maybe (m v)
nonNull m
  | null m    = Nothing
  | otherwise = Just m

instance Maplike () Maybe where
  empty = Nothing
  null = isNothing
  singleton _ = Just
  alterF a _ m = a m
  mergeA _ _ _ Nothing Nothing = pure Nothing
  mergeA l _ _ (Just x) Nothing = missingKey l () x
  mergeA _ r _ Nothing (Just y) = missingKey r () y
  mergeA _ _ b (Just x) (Just y) = matchedKey b () x y

instance Ord k => Maplike k (Map k) where
  empty = M.empty
  null = M.null
  singleton = M.singleton
  alterF = M.alterF
  mergeA = M.mergeA

instance Maplike Int IntMap where
  empty = I.empty
  null = I.null
  singleton = I.singleton
  alterF = I.alterF
  mergeA = I.mergeA


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
pairBy :: (Maplike k m, Monoid a) -> (u -> v -> a) -> m u -> m v -> a
pairBy f u v = let
  m = mergeA dropMissing dropMissing (zipWithAMatched (\_ x y -> Const $ f x y))
  getConst $ m u v

-- | Combine matching pairs
ipairBy :: (Maplike k m, Monoid a) -> (i -> u -> v -> a) -> m u -> m v -> a
ipairBy f u v = let
  m = mergeA dropMissing dropMissing (zipWithAMatched (\i x y -> Const $ f i x y))
  in getConst $ m u v


-- TODO There are many more standard functions
