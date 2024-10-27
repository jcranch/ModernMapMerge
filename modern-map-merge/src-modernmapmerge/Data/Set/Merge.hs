-- | Merges involving `Data.Set` and perhaps `Data.Map`.
--
-- This is more speculative than the merges for `Data.Map`.
module Data.Set.Merge where

import Control.Applicative (liftA3)
import Data.Map.Internal hiding (WhenMissing(..),
                                 runWhenMissing,
                                 WhenMatched(..),
                                 SimpleWhenMissing,
                                 SimpleWhenMatched)
import Data.MergeTactics        (WhenMissing(..),
                                 runWhenMissing,
                                 missingKey,
                                 WhenMatched(..),
                                 SimpleWhenMissing,
                                 SimpleWhenMatched)
import Data.Set.Internal        (Set)

import qualified Data.Map.Internal as M
import qualified Data.Set.Internal as S

import Data.Set.Merge.Internal


-- | The general combining function for `Set`
mergeSetA
  :: (Applicative f, Ord k)
  => WhenMissing f k () () -- ^ What to do with keys in @s1@ but not @s2@
  -> WhenMissing f k () () -- ^ What to do with keys in @s2@ but not @s1@
  -> WhenMatched f k () () () -- ^ What to do with keys in both @s1@ and @s2@
  -> Set k -- ^ Set @s1@
  -> Set k -- ^ Set @s2@
  -> f (Set k)
mergeSetA w1 w2 (WhenMatched f) = let

  -- It might well be more efficient to use pointer equality to not
  -- make new nodes.
  assemble _ Nothing  = S.merge
  assemble k (Just _) = S.link k

  go t1 S.Tip = getSet <$> runWhenMissing w1 (Set' t1)
  go S.Tip t2 = getSet <$> runWhenMissing w2 (Set' t2)
  go (S.Bin _ k l1 r1) t2 = let
    (l2, m, r2) = S.splitMember k t2
    x = case m of
      True -> f k () ()
      False -> missingKey w1 k ()
    l = go l1 l2
    r = go r1 r2
    in liftA3 (assemble k) x l r

  in go

-- | A nonapplicative version of the above
mergeSet
  :: Ord k
  => SimpleWhenMissing k () () -- ^ What to do with keys in @s1@ but not @s2@
  -> SimpleWhenMissing k () () -- ^ What to do with keys in @s2@ but not @s1@
  -> SimpleWhenMatched k () () () -- ^ What to do with keys in both @s1@ and @s2@
  -> Set k -- ^ Set @s1@
  -> Set k -- ^ Set @s2@
  -> Set k
mergeSet w1 w2 w s1 s2 = let
  c = mergeSetA w1 w2 w
  in runIdentity $ c s1 s2

mergeMapAndSetToSetA
  :: (Applicative f, Ord k)
  => WhenMissing f k a () -- ^ What to do with keys in @m@ but not @s@
  -> WhenMissing f k () () -- ^ What to do with keys in @s@ but not @m@
  -> WhenMatched f k a () () -- ^ What to do with keys in both @s@ and @m@
  -> Map k a -- ^ Map @m@
  -> Set k -- ^ Set @s@
  -> f (Set k)
mergeMapAndSetToSetA w1 w2 (WhenMatched f) = let

  -- Again, it might well be more efficient to use pointer equality to
  -- not make new nodes.
  assemble _ Nothing  = S.merge
  assemble k (Just _) = S.link k

  go M.Tip t2 = getSet <$> runWhenMissing w2 (Set' t2)

  -- We have an irritating choice here. What we choose here risks
  -- traversing the tree structure twice, but allows the possibility
  -- of shortcutting (if w1 is dropWhenMissing, for example). We could
  -- alternatively choose to traverse the tree only once, without
  -- shortcutting.
  go t1 S.Tip = M.keysSet <$> runWhenMissing w1 t1

  go (M.Bin _ k x1 l1 r1) t2 = let
    (l2, m, r2) = S.splitMember k t2
    x = case m of
      True -> f k x1 ()
      False -> missingKey w1 k x1
    l = go l1 l2
    r = go r1 r2
    in liftA3 (assemble k) x l r

  in go

-- | A nonapplicative version of the above
mergeMapAndSetToSet
  :: Ord k
  => SimpleWhenMissing k a () -- ^ What to do with keys in @m@ but not @s@
  -> SimpleWhenMissing k () () -- ^ What to do with keys in @s@ but not @m@
  -> SimpleWhenMatched k a () () -- ^ What to do with keys in both @s@ and @m@
  -> Map k a -- ^ Map @m@
  -> Set k -- ^ Set @s@
  -> Set k
mergeMapAndSetToSet w1 w2 w m s = let
  c = mergeMapAndSetToSetA w1 w2 w
  in runIdentity $ c m s

mergeSetAndMapToMapA
  :: (Applicative f, Ord k)
  => WhenMissing f k () c -- ^ What to do with keys in @s@ but not @m@
  -> WhenMissing f k b c -- ^ What to do with keys in @m@ but not @s@
  -> WhenMatched f k () b c -- ^ What to do with keys in both @s@ and @m@
  -> Set k -- ^ Set @s@
  -> Map k b -- ^ Map @m@
  -> f (Map k c)
mergeSetAndMapToMapA w1 w2 (WhenMatched f) = let

  go S.Tip t2 = runWhenMissing w2 t2

  -- Here we don't have the irritating choice described above; our
  -- approach to making Set an instance of Witherable doesn't allow us
  -- to put keys into it, so we must traverse the tree once (but we
  -- provide a specialised method that ignores the second variable in
  -- that traversal).
  go t1 M.Tip = go1 t1

  go (S.Bin _ k l1 r1) t2 = let
    (l2, mx, r2) = M.splitLookup k t2
    x = case mx of
      Just x2 -> f k () x2
      Nothing -> missingKey w1 k ()
    l = go l1 l2
    r = go r1 r2
    in liftA3 (maybe link2 (link k)) x l r

  go1 S.Tip = pure M.Tip
  go1 (S.Bin _ k l r) = liftA3 (maybe link2 (link k)) (missingKey w1 k ()) (go1 l) (go1 r)

  in go

mergeSetAndMapToMap
  :: Ord k
  => SimpleWhenMissing k () c -- ^ What to do with keys in @s@ but not @m@
  -> SimpleWhenMissing k b c -- ^ What to do with keys in @m@ but not @s@
  -> SimpleWhenMatched k () b c -- ^ What to do with keys in both @s@ and @m@
  -> Set k -- ^ Set @s@
  -> Map k b -- ^ Map @m@
  -> Map k c
mergeSetAndMapToMap w1 w2 w s m = let
  c = mergeSetAndMapToMapA w1 w2 w
  in runIdentity $ c s m
