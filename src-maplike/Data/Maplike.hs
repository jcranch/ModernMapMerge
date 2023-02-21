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

  mergeA :: (Applicative f)
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
