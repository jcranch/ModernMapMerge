{-# LANGUAGE
      BangPatterns
  #-}


-- | Merging for `Data.Map` in this new framework
module Data.Map.Merge where

import Control.Applicative (liftA3)
import Data.Map.Internal hiding (WhenMissing(..),
                                 SimpleWhenMissing,
                                 runWhenMissing,
                                 WhenMatched(..),
                                 SimpleWhenMatched,
                                 mergeA)
import Data.MergeTactics        (WhenMissing(..),
                                 SimpleWhenMissing,
                                 runWhenMissing,
                                 WhenMatched(..),
                                 SimpleWhenMatched,
                                 missingKey)


-- | The general combining function for `Map`.
mergeA
  :: (Applicative f, Ord k)
  => WhenMissing f k a c -- ^ What to do with keys in @m1@ but not @m2@
  -> WhenMissing f k b c -- ^ What to do with keys in @m2@ but not @m1@
  -> WhenMatched f k a b c -- ^ What to do with keys in both @m1@ and @m2@
  -> Map k a -- ^ Map @m1@
  -> Map k b -- ^ Map @m2@
  -> f (Map k c)
mergeA
    w1
    w2
    (WhenMatched f) = go
  where
    g1k = missingKey w1
    g1t = runWhenMissing w1
    g2t = runWhenMissing w2
    go t1 Tip = g1t t1
    go Tip t2 = g2t t2
    go (Bin _ kx x1 l1 r1) t2 = case splitLookup kx t2 of
      (l2, mx2, r2) -> case mx2 of
          Nothing -> liftA3 (\l' mx' r' -> maybe link2 (link kx) mx' l' r')
                        l1l2 (g1k kx x1) r1r2
          Just x2 -> liftA3 (\l' mx' r' -> maybe link2 (link kx) mx' l' r')
                        l1l2 (f kx x1 x2) r1r2
        where
          !l1l2 = go l1 l2
          !r1r2 = go r1 r2
{-# INLINE mergeA #-}

-- | A simpler map merge
merge
  :: (Ord k)
  => SimpleWhenMissing k a c -- ^ What to do with keys in @m1@ but not @m2@
  -> SimpleWhenMissing k b c -- ^ What to do with keys in @m2@ but not @m1@
  -> SimpleWhenMatched k a b c -- ^ What to do with keys in both @m1@ and @m2@
  -> Map k a -- ^ Map @m1@
  -> Map k b -- ^ Map @m2@
  -> Map k c
merge onL onR onB m1 m2 = let
  f = mergeA onL onR onB
  in runIdentity $ f m1 m2

{-# INLINE merge #-}
