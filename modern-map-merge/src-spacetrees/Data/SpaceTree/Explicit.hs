{-# LANGUAGE
      DeriveFunctor,
      FlexibleContexts,
      FlexibleInstances,
      FunctionalDependencies,
      MultiParamTypeClasses,
      QuantifiedConstraints
  #-}

-- | An interface to spacetrees where we supply the bounding box with
-- all instructions
module Data.SpaceTree.Explicit where

import Data.Foldable.WithIndex
import Data.Functor.WithIndex
import qualified Data.PQueue.Prio.Min as Q
import Data.Monoid (Sum(..))
import Data.Ord (Down(..))
import Data.Traversable.WithIndex
import Witherable

import Data.Maplike

import Data.MergeTactics
import Data.SpaceTree.Coords


data SpaceTree p i b m v =
  Empty |
  Singleton p v |
  Branch Int (m (SpaceTree p i b m v))
  deriving (Functor)

classifyT :: SpaceTree p i b m v -> Classified p v
classifyT Empty           = Zero
classifyT (Singleton p v) = One p v
classifyT (Branch _ _)    = Many

nonNullT :: SpaceTree p i b m v -> Maybe (SpaceTree p i b m v)
nonNullT Empty = Nothing
nonNullT m     = Just m

sizeT :: SpaceTree p i b m v -> Int
sizeT Empty           = 0
sizeT (Singleton _ _) = 1
sizeT (Branch n _)    = n

makeBranch :: (Maplike i m) => m (SpaceTree p i b m v) -> SpaceTree p i b m v
makeBranch m = case bindClassify (const id) classifyT (classify m) of
  Zero    -> Empty
  One p v -> Singleton p v
  Many    -> Branch (getSum $ foldMap (Sum . sizeT) m) m

nullT :: SpaceTree p i b m v -> Bool
nullT Empty = True
nullT _     = False

maybeSingleton :: p -> Maybe v -> SpaceTree p i b m v
maybeSingleton _ Nothing  = Empty
maybeSingleton p (Just v) = Singleton p v

doubleton :: (Coordinate b p i, Maplike i m) => b -> p -> v -> p -> v -> SpaceTree p i b m v
doubleton b p1 v1 p2 v2 = let
  (i1, b') = narrow b p1
  (i2, _ ) = narrow b p2
  in if i1 == i2
     then doubleton b' p1 v1 p2 v2
     else Branch 2 $ fromFold [(i1,Singleton p1 v1),(i2,Singleton p2 v2)]

maybeDoubleton :: (Coordinate b p i, Maplike i m) => b -> p -> v -> p -> Maybe v -> SpaceTree p i b m v
maybeDoubleton b p1 v1 p2 (Just v2) = doubleton b p1 v1 p2 v2
maybeDoubleton _ p1 v1 _  Nothing   = Singleton p1 v1

maybeDoubleton2 :: (Coordinate b p i, Maplike i m) => b -> p -> p -> Maybe v -> Maybe v -> SpaceTree p i b m v
maybeDoubleton2 _ _  _  Nothing   Nothing   = Empty
maybeDoubleton2 _ _  p2 Nothing   (Just v2) = Singleton p2 v2
maybeDoubleton2 _ p1 _  (Just v1) Nothing   = Singleton p1 v1
maybeDoubleton2 b p1 p2 (Just v1) (Just v2) = doubleton b p1 v1 p2 v2


alterFT :: (Coordinate b p i,
            Maplike i m,
            Functor f)
        => (Maybe v -> f (Maybe v))
        -> p
        -> b
        -> SpaceTree p i b m v
        -> f (SpaceTree p i b m v)
alterFT f p = let
  go _ Empty = maybeSingleton p <$> f Nothing
  go b (Singleton q x)
    | p == q    = maybeSingleton q <$> f (Just x)
    | otherwise = maybeDoubleton b q x p <$> f Nothing
  go b (Branch _ u) = let
    (i,b') = narrow b p
    h Nothing  = fmap (Singleton p) <$> f Nothing
    h (Just m) = nonNullT <$> go b' m
    in makeBranch <$> alterF h i u
  in go

-- | I think I need to be careful with strictness in this algorithm
alterBestWithKeyFT :: (Coordinate b p i,
                       Maplike i m,
                       Ord a,
                       Functor f)
                   => (p -> Maybe a)
                   -> (b -> Maybe a)
                   -> (p -> v -> f (Maybe v))
                   -> b
                   -> SpaceTree p i b m v
                   -> Maybe (f (SpaceTree p i b m v))
alterBestWithKeyFT r s f = let
  enqueue b c m q = case m of
    Empty         -> q
    Singleton p v -> case r p of
      Just x  -> Q.insert x (Left (p, v), c) q
      Nothing -> q
    Branch _ u    -> case s b of
      Just x  -> Q.insert x (Right (b, u), c) q
      Nothing -> q
  go q = case Q.minView q of
    Nothing                      -> Nothing
    Just ((Left (p, v), c), _)   -> Just (c . maybeSingleton p <$> f p v)
    Just ((Right (b, u), c), q') -> go $ ifoldr (\i -> enqueue (subbox b i) (c . makeBranch . flip (insert i) u)) q' u
  start b m = go $ enqueue b id m Q.empty
  in start


alterMinWithKeyFT :: (Coordinate b p i,
                      Maplike i m,
                      Functor f)
                  => (p -> v -> f (Maybe v))
                  -> b
                  -> SpaceTree p i b m v
                  -> Maybe (f (SpaceTree p i b m v))
alterMinWithKeyFT = alterBestWithKeyFT Just (Just . leastPoint)


alterMaxWithKeyFT :: (Coordinate b p i,
                      Maplike i m,
                      Functor f)
                  => (p -> v -> f (Maybe v))
                  -> b
                  -> SpaceTree p i b m v
                  -> Maybe (f (SpaceTree p i b m v))
alterMaxWithKeyFT = alterBestWithKeyFT (Just . Down) (Just . Down . greatestPoint)


alterAnyWithKeyFT :: (Coordinate b p i,
                      Maplike i m,
                      Functor f)
                  => (p -> v -> f (Maybe v))
                  -> SpaceTree p i b m v
                  -> Maybe (f (SpaceTree p i b m v))
alterAnyWithKeyFT _ Empty           = Nothing
alterAnyWithKeyFT f (Singleton p v) = Just (maybeSingleton p <$> f p v)
alterAnyWithKeyFT f (Branch _ u)    = let
  h (Just m) = m
  h Nothing  = error "alterAnyWithKeyFT: unexpected Nothing"
  in fmap makeBranch <$> alterAnyF (fmap nonNullT . h . alterAnyWithKeyFT f) u


foldlBestWithKeyT :: (Coordinate b p i,
                      Maplike i m,
                      Ord a)
                  => (p -> Maybe a)
                  -> (b -> Maybe a)
                  -> (c -> p -> v -> c)
                  -> c
                  -> b
                  -> SpaceTree p i b m v
                  -> c
foldlBestWithKeyT r s f = let
  enqueue b m q = case m of
    Empty         -> q
    Singleton p v -> case r p of
      Just x  -> Q.insert x (Left (p, v)) q
      Nothing -> q
    Branch _ u    -> case s b of
      Just x  -> Q.insert x (Right (b, u)) q
      Nothing -> q
  go z q = case Q.minView q of
    Nothing                 -> z
    Just (Left (p, v), q')  -> go (f z p $! v) q'
    Just (Right (b, u), q') -> go z $ ifoldr (enqueue . subbox b) q' u
  start z b m = go z $ enqueue b m Q.empty
  in start


foldrBestWithKeyT :: (Coordinate b p i,
                      Maplike i m,
                      Ord a)
                  => (p -> Maybe a)
                  -> (b -> Maybe a)
                  -> (p -> v -> c -> c)
                  -> c
                  -> b
                  -> SpaceTree p i b m v
                  -> c
foldrBestWithKeyT r s f z = let
  enqueue b m q = case m of
    Empty         -> q
    Singleton p v -> case r p of
      Just x  -> Q.insert x (Left (p, v)) q
      Nothing -> q
    Branch _ u    -> case s b of
      Just x  -> Q.insert x (Right (b, u)) q
      Nothing -> q
  go q = case Q.minView q of
    Nothing                 -> z
    Just (Left (p, v), q')  -> f p v $ go q'
    Just (Right (b, u), q') -> go $ ifoldr (enqueue . subbox b) q' u
  start b m = go $ enqueue b m Q.empty
  in start


instance (Eq p, forall x. Eq x => Eq (m x), Eq v) => Eq (SpaceTree p i b m v) where
  Empty           == Empty           = True
  Singleton p1 v1 == Singleton p2 v2 = p1 == p2 && v1 == v2
  Branch n1 m1    == Branch n2 m2    = n1 == n2 && m1 == m2 -- testing size is faster
  _               == _               = False


instance Functor m => FunctorWithIndex p (SpaceTree p i b m) where
  imap _ Empty = Empty
  imap f (Singleton p v) = Singleton p $ f p v
  imap f (Branch n u) = Branch n $ fmap (imap f) u

instance Foldable m => Foldable (SpaceTree p i b m) where
  foldr _ z Empty = z
  foldr f z (Singleton _ a) = f a z
  foldr f z (Branch _ u) = foldr (flip (foldr f)) z u
  foldMap _ Empty = mempty
  foldMap f (Singleton _ a) = f a
  foldMap f (Branch _ u) = foldMap (foldMap f) u

instance Foldable m => FoldableWithIndex p (SpaceTree p i b m) where
  ifoldr _ z Empty = z
  ifoldr f z (Singleton p a) = f p a z
  ifoldr f z (Branch _ u) = foldr (flip (ifoldr f)) z u
  ifoldMap _ Empty = mempty
  ifoldMap f (Singleton p a) = f p a
  ifoldMap f (Branch _ u) = foldMap (ifoldMap f) u

instance Traversable m => Traversable (SpaceTree p i b m) where
  traverse _ Empty = pure Empty
  traverse f (Singleton p a) = Singleton p <$> f a
  traverse f (Branch n u) = Branch n <$> traverse (traverse f) u

instance Traversable m => TraversableWithIndex p (SpaceTree p i b m) where
  itraverse _ Empty = pure Empty
  itraverse f (Singleton p a) = Singleton p <$> f p a
  itraverse f (Branch n u) = Branch n <$> traverse (itraverse f) u

instance (Maplike i m) => Filterable (SpaceTree p i b m) where
  mapMaybe _ Empty = Empty
  mapMaybe f (Singleton p a) = maybeSingleton p (f a)
  mapMaybe f (Branch _ u) = makeBranch $ fmap (mapMaybe f) u

instance (Maplike i m) => FilterableWithIndex p (SpaceTree p i b m) where
  imapMaybe _ Empty = Empty
  imapMaybe f (Singleton p a) = maybeSingleton p (f p a)
  imapMaybe f (Branch _ u) = makeBranch $ mapMaybe (nonNullT . imapMaybe f) u

instance (Maplike i m) => Witherable (SpaceTree p i b m) where
  wither _ Empty = pure Empty
  wither f (Singleton p a) = maybeSingleton p <$> f a
  wither f (Branch _ u) = makeBranch <$> wither (fmap nonNullT . wither f) u

instance (Maplike i m) => WitherableWithIndex p (SpaceTree p i b m) where
  iwither _ Empty = pure Empty
  iwither f (Singleton p a) = maybeSingleton p <$> f p a
  iwither f (Branch _ u) = makeBranch <$> wither (fmap nonNullT . iwither f) u


fastFilter :: (Coordinate b p i, Maplike i m) => (p -> Bool) -> (b -> Maybe Bool) -> b -> SpaceTree p i b m v -> SpaceTree p i b m v
fastFilter f g = let
  go _ Empty = Empty
  go _ s@(Singleton p _) = if f p then s else Empty
  go b s@(Branch _ u) = case g b of
    Just True  -> s
    Just False -> Empty
    Nothing    -> makeBranch $ imapMaybe (\i -> nonNullT . go (subbox b i)) u
  in go

fastFilterA :: (Coordinate b p i, Maplike i m, Monad f) => (p -> f Bool) -> (b -> f (Maybe Bool)) -> b -> SpaceTree p i b m v -> f (SpaceTree p i b m v)
fastFilterA f g = let
  go _ Empty = pure Empty
  go _ s@(Singleton p _) = let
    h z = if z then s else Empty
    in h <$> f p
  go b s@(Branch _ u) = let
    h (Just True)  = pure s
    h (Just False) = pure Empty
    h Nothing      = makeBranch <$> iwither (\i -> fmap nonNullT . go (subbox b i)) u
    in h =<< g b
  in go


extent :: (Coordinate b p i, Foldable m) => SpaceTree p i b m v -> Maybe b
extent = ifoldMap (\p _ -> Just (pointBox p))


-- | Merge, assuming all trees involved have coinciding bounding boxes
imergeSameT :: (Coordinate b p i,
                Maplike i m)
            => SimpleWhenMissing p u w
            -> SimpleWhenMissing p v w
            -> SimpleWhenMatched p u v w
            -> b
            -> SpaceTree p i b m u
            -> SpaceTree p i b m v
            -> SpaceTree p i b m w
imergeSameT l r t = let

  pair b u1 u2 = let
    l' = umapMaybeMissing (nonNullT . runSimpleWhenMissing l)
    r' = umapMaybeMissing (nonNullT . runSimpleWhenMissing r)
    t' i v1 v2 = nonNullT $ go (subbox b i) v1 v2
    in makeBranch $ imerge l' r' (zipWithMaybeMatched t') u1 u2

  go _ Empty               m2                  = runSimpleWhenMissing r m2
  go _ m1                  Empty               = runSimpleWhenMissing l m1
  go b (Singleton p1 v1)   (Singleton p2 v2)
    | p1 == p2                                 = maybeSingleton p1 $ simpleMatchedKey t p1 v1 v2
    | otherwise                                = maybeDoubleton2 b p1 p2
                                                   (simpleMissingKey l p1 v1)
                                                   (simpleMissingKey r p2 v2)
  go b (Branch _ u1)       (Branch _ u2)       = pair b u1 u2
  go b s1@(Singleton p1 _) (Branch _ u2)       = let
    (i1, _) = narrow b p1
    in pair b (singleton i1 s1) u2
  go b (Branch _ u1)       s2@(Singleton p2 _) = let
    (i2, _) = narrow b p2
    in pair b u1 (singleton i2 s2)

  in go


-- | Merge, assuming all trees involved have coinciding bounding boxes
imergeSameAT :: (Coordinate b p i,
                 Maplike i m,
                 Applicative f)
             => WhenMissing f p u w
             -> WhenMissing f p v w
             -> WhenMatched f p u v w
             -> b
             -> SpaceTree p i b m u
             -> SpaceTree p i b m v
             -> f (SpaceTree p i b m w)
imergeSameAT l r t = let

  pair b u1 u2 = let
    l' = utraverseMaybeMissing (fmap nonNullT . runWhenMissing l)
    r' = utraverseMaybeMissing (fmap nonNullT . runWhenMissing r)
    t' i v1 v2 = nonNullT <$> go (subbox b i) v1 v2
    in makeBranch <$> imergeA l' r' (WhenMatched t') u1 u2

  go _ Empty               m2                  = runWhenMissing r m2
  go _ m1                  Empty               = runWhenMissing l m1
  go b (Singleton p1 v1)   (Singleton p2 v2)
    | p1 == p2                                 = maybeSingleton p1 <$> matchedKey t p1 v1 v2
    | otherwise                                = liftA2
                                                   (maybeDoubleton2 b p1 p2)
                                                   (missingKey l p1 v1)
                                                   (missingKey r p2 v2)
  go b (Branch _ u1)       (Branch _ u2)       = pair b u1 u2
  go b s1@(Singleton p1 _) (Branch _ u2)       = let
    (i1, _) = narrow b p1
    in pair b (singleton i1 s1) u2
  go b (Branch _ u1)       s2@(Singleton p2 _) = let
    (i2, _) = narrow b p2
    in pair b u1 (singleton i2 s2)

  in go

{-

-- | Return parts contained within the new box, and the
-- remainder. This is probably only of interest insofar as it's part
-- of the implementation of "rebound".
clipRebound :: b -> [(b, SpaceTree p i b m v)] -> (SpaceTree p i b m v, [(b, SpaceTree p i b m v)])
clipRebound _ [] = (Empty, [])
clipRebound b [(c, Singleton p v)] = if containsPoint b p
  then (Singleton p v, [])
  else (Empty, [(c, Singleton p v)])
clipRebound b = let
  inner ((c, t):l) = if disjoint b c then
    case getAnyWithKey t of
    Nothing    -> inner l
    Just (p,v) -> if containsPoint b p
      then let
      (i,b') = narrow b p
      (n,l') = clipRebound

-- | Change bounding box, assume (and do not check) that all points
-- are in the new bounding box
rebound :: b -> [(b, SpaceTree p i b m v)] -> SpaceTree p i b m v
rebound _ [] = Empty
rebound _ [(_, Singleton p v)] = Singleton p v
rebound new ((old, t):l) = case anyViewWithKey t of
  Nothing -> rebound new l

-}
