{-# LANGUAGE
      BangPatterns,
      CPP
  #-}


-- | Merging for `IntMap` in this new framework
module Data.IntMap.MergeGeneric where

#if MIN_VERSION_base(4,18,0)
#else
import Control.Applicative (liftA2)
#endif
import Data.IntMap.Internal hiding (WhenMissing(..),
                                    runWhenMissing,
                                    WhenMatched(..))
import Data.MergeTactics           (WhenMissing(..),
                                    runWhenMissing,
                                    missingKey,
                                    WhenMatched(..))


-- | The general combining function for `IntMap`.
mergeA
  :: (Applicative f)
  => WhenMissing f Int a c -- ^ What to do with keys in @m1@ but not @m2@
  -> WhenMissing f Int b c -- ^ What to do with keys in @m2@ but not @m1@
  -> WhenMatched f Int a b c -- ^ What to do with keys in both @m1@ and @m2@
  -> IntMap a -- ^ Map @m1@
  -> IntMap b -- ^ Map @m2@
  -> f (IntMap c)
mergeA
    w1
    w2
    WhenMatched{matchedKey = f}
    = go
  where
    g1t = runWhenMissing w1
    g1k = missingKey w1
    g2t = runWhenMissing w2
    g2k = missingKey w2

    go t1  Nil = g1t t1
    go Nil t2  = g2t t2

    -- This case is already covered below.
    -- go (Tip k1 x1) (Tip k2 x2) = mergeTips k1 x1 k2 x2

    go (Tip k1 x1) t2' = merge2 t2'
      where
        merge2 t2@(Bin p2 m2 l2 r2)
          | nomatch k1 p2 m2 = linkA k1 (subsingletonBy g1k k1 x1) p2 (g2t t2)
          | zero k1 m2       = binA p2 m2 (merge2 l2) (g2t r2)
          | otherwise        = binA p2 m2 (g2t l2) (merge2 r2)
        merge2 (Tip k2 x2)   = mergeTips k1 x1 k2 x2
        merge2 Nil           = subsingletonBy g1k k1 x1

    go t1' (Tip k2 x2) = merge1 t1'
      where
        merge1 t1@(Bin p1 m1 l1 r1)
          | nomatch k2 p1 m1 = linkA p1 (g1t t1) k2 (subsingletonBy g2k k2 x2)
          | zero k2 m1       = binA p1 m1 (merge1 l1) (g1t r1)
          | otherwise        = binA p1 m1 (g1t l1) (merge1 r1)
        merge1 (Tip k1 x1)   = mergeTips k1 x1 k2 x2
        merge1 Nil           = subsingletonBy g2k k2 x2

    go t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
      | shorter m1 m2  = merge1
      | shorter m2 m1  = merge2
      | p1 == p2       = binA p1 m1 (go l1 l2) (go r1 r2)
      | otherwise      = linkA p1 (g1t t1) p2 (g2t t2)
      where
        merge1 | nomatch p2 p1 m1  = linkA p1 (g1t t1) p2 (g2t t2)
               | zero p2 m1        = binA p1 m1 (go  l1 t2) (g1t r1)
               | otherwise         = binA p1 m1 (g1t l1)    (go  r1 t2)
        merge2 | nomatch p1 p2 m2  = linkA p1 (g1t t1) p2 (g2t t2)
               | zero p1 m2        = binA p2 m2 (go  t1 l2) (g2t    r2)
               | otherwise         = binA p2 m2 (g2t    l2) (go  t1 r2)

    subsingletonBy gk k x = maybe Nil (Tip k) <$> gk k x
    {-# INLINE subsingletonBy #-}

    mergeTips k1 x1 k2 x2
      | k1 == k2  = maybe Nil (Tip k1) <$> f k1 x1 x2
      | k1 <  k2  = liftA2 (subdoubleton k1 k2) (g1k k1 x1) (g2k k2 x2)
        {-
        = link_ k1 k2 <$> subsingletonBy g1k k1 x1 <*> subsingletonBy g2k k2 x2
        -}
      | otherwise = liftA2 (subdoubleton k2 k1) (g2k k2 x2) (g1k k1 x1)
    {-# INLINE mergeTips #-}

    subdoubleton _ _   Nothing Nothing     = Nil
    subdoubleton _ k2  Nothing (Just y2)   = Tip k2 y2
    subdoubleton k1 _  (Just y1) Nothing   = Tip k1 y1
    subdoubleton k1 k2 (Just y1) (Just y2) = link k1 (Tip k1 y1) k2 (Tip k2 y2)
    {-# INLINE subdoubleton #-}

    -- | A variant of 'link_' which makes sure to execute side-effects
    -- in the right order.
    linkA
        :: Applicative f
        => Prefix -> f (IntMap a)
        -> Prefix -> f (IntMap a)
        -> f (IntMap a)
    linkA p1 t1 p2 t2
      | zero p1 m = binA p m t1 t2
      | otherwise = binA p m t2 t1
      where
        m = branchMask p1 p2
        p = mask p1 m
    {-# INLINE linkA #-}

    -- A variant of 'bin' that ensures that effects for negative keys are executed
    -- first.
    binA
        :: Applicative f
        => Prefix
        -> Mask
        -> f (IntMap a)
        -> f (IntMap a)
        -> f (IntMap a)
    binA p m a b
      | m < 0     = liftA2 (flip (bin p m)) b a
      | otherwise = liftA2       (bin p m)  a b
    {-# INLINE binA #-}
{-# INLINE mergeA #-}
