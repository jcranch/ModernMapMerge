{-# LANGUAGE
      DeriveFunctor,
      DerivingStrategies,
      DerivingVia,
      FlexibleContexts,
      FlexibleInstances,
      FunctionalDependencies,
      GeneralizedNewtypeDeriving,
      MultiParamTypeClasses,
      QuantifiedConstraints
  #-}

-- | An interface to spacetrees where we supply the bounding box with
-- all instructions
module Data.SpaceTree.Explicit where

import Control.Monad (join)
import Data.Complex (Complex(..))
import Data.Foldable.WithIndex
import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))
import Data.Functor.WithIndex
import Data.Monoid (Sum(..))
import qualified Data.PQueue.Prio.Min as Q
import Data.Ord (Down(..))
import Data.Traversable.WithIndex
import Data.Vector (Vector)
import qualified Data.Vector as V
import Witherable

import Data.Classified
import Data.Maplike

import Data.MergeTactics
import Data.SpaceTree.Coords


class Monoid a => Measure a p where
  measure :: p -> a

newtype BoxMeasure b p i = BoxMeasure {
  getBox :: Maybe b
} deriving (Eq, Show)
  deriving (Semigroup, Monoid) via (Maybe b)

instance Coordinate b p i => Measure (BoxMeasure b p i) p where
  measure = BoxMeasure . Just . pointBox

newtype Counting p = Counting {
  getCount :: Int
} deriving (Eq, Show)
  deriving (Semigroup, Monoid) via Sum Int

instance Measure (Counting p) p where
  measure _ = Counting 1

data PairMeasure a b p = PairMeasure {
  measureL :: a,
  measureR :: b
} deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (PairMeasure a b p) where
  PairMeasure x1 y1 <> PairMeasure x2 y2 = PairMeasure (x1 <> x2) (y1 <> y2)

instance (Monoid a, Monoid b) => Monoid (PairMeasure a b p) where
  mempty = PairMeasure mempty mempty

instance (Measure a p, Measure b p) => Measure (PairMeasure a b p) p where
  measure p = PairMeasure (measure p) (measure p)


data SpaceTree a p i b m v =
  Empty |
  Singleton p v |
  Branch a (m (SpaceTree a p i b m v))
  deriving (Functor)

classifyT :: SpaceTree a p i b m v -> Classified p v
classifyT Empty           = Zero
classifyT (Singleton p v) = One p v
classifyT (Branch _ _)    = Many

nonNullT :: SpaceTree a p i b m v -> Maybe (SpaceTree a p i b m v)
nonNullT Empty = Nothing
nonNullT m     = Just m

instance Measure a p => Measure a (SpaceTree a p i b m v) where
  measure Empty           = mempty
  measure (Singleton p _) = measure p
  measure (Branch n _)    = n

-- | This avoids some problems caused by ambiguous instances
totalMeasure :: Measure a p => SpaceTree a p i b m v -> a
totalMeasure = measure

-- | Assumes there are at least two points
makeBranchUnsafe :: (Maplike i m, Measure a p) => m (SpaceTree a p i b m v) -> SpaceTree a p i b m v
makeBranchUnsafe m = Branch (foldMap measure m) m

-- | Checks whether there are zero, one, or two-or-more points
makeBranch :: (Maplike i m, Measure a p) => m (SpaceTree a p i b m v) -> SpaceTree a p i b m v
makeBranch m = case bindClassify (const id) classifyT (classify m) of
  Zero    -> Empty
  One p v -> Singleton p v
  Many    -> makeBranchUnsafe m

nullT :: SpaceTree a p i b m v -> Bool
nullT Empty = True
nullT _     = False

maybeSingletonT :: p -> Maybe v -> SpaceTree a p i b m v
maybeSingletonT _ Nothing  = Empty
maybeSingletonT p (Just v) = Singleton p v

-- | Assumes that the two points are different; computes the measure
doubleton :: (Coordinate b p i, Maplike i m, Measure a p) => b -> p -> v -> p -> v -> SpaceTree a p i b m v
doubleton b p1 v1 p2 v2 = doubleton' b (measure p1 <> measure p2) p1 v1 p2 v2

-- | Assumes that the two points are different; takes the measure precomputed
doubleton' :: (Coordinate b p i, Maplike i m, Measure a p) => b -> a -> p -> v -> p -> v -> SpaceTree a p i b m v
doubleton' b m p1 v1 p2 v2 = let
  (i1, b') = narrow b p1
  (i2, _ ) = narrow b p2
  in Branch m $ if i1 == i2
     then singleton i1 $ doubleton' b' m p1 v1 p2 v2
     else fromFold [(i1,Singleton p1 v1),(i2,Singleton p2 v2)]

maybeDoubleton :: (Coordinate b p i, Maplike i m, Measure a p) => b -> p -> v -> p -> Maybe v -> SpaceTree a p i b m v
maybeDoubleton b p1 v1 p2 (Just v2) = doubleton b p1 v1 p2 v2
maybeDoubleton _ p1 v1 _  Nothing   = Singleton p1 v1

maybeDoubleton2 :: (Coordinate b p i, Maplike i m, Measure a p) => b -> p -> p -> Maybe v -> Maybe v -> SpaceTree a p i b m v
maybeDoubleton2 _ _  _  Nothing   Nothing   = Empty
maybeDoubleton2 _ _  p2 Nothing   (Just v2) = Singleton p2 v2
maybeDoubleton2 _ p1 _  (Just v1) Nothing   = Singleton p1 v1
maybeDoubleton2 b p1 p2 (Just v1) (Just v2) = doubleton b p1 v1 p2 v2


alterFT :: (Coordinate b p i,
            Maplike i m,
            Measure a p,
            Functor f)
        => (Maybe v -> f (Maybe v))
        -> p
        -> b
        -> SpaceTree a p i b m v
        -> f (SpaceTree a p i b m v)
alterFT f p = let
  go _ Empty = maybeSingletonT p <$> f Nothing
  go b (Singleton q x)
    | p == q    = maybeSingletonT q <$> f (Just x)
    | otherwise = maybeDoubleton b q x p <$> f Nothing
  go b (Branch _ u) = let
    (i,b') = narrow b p
    h Nothing  = fmap (Singleton p) <$> f Nothing
    h (Just m) = nonNullT <$> go b' m
    in makeBranch <$> alterF h i u
  in go

maybeInsertT :: (Coordinate b p i,
                 Maplike i m,
                 Measure a p)
             => p
             -> b
             -> Maybe v
             -> SpaceTree a p i b m v
             -> SpaceTree a p i b m v
maybeInsertT _ _ Nothing    = id
maybeInsertT p b v@(Just _) = runIdentity . alterFT (const $ Identity v) p b

-- | I think I need to be careful with strictness in this algorithm
alterBestWithKeyFT :: (Coordinate b p i,
                       Maplike i m,
                       Measure a p,
                       Ord o,
                       Functor f)
                   => (p -> Maybe o)
                   -> (a -> b -> Maybe o)
                   -> (o -> p -> v -> f (Maybe v))
                   -> b
                   -> SpaceTree a p i b m v
                   -> Maybe (f (SpaceTree a p i b m v))
alterBestWithKeyFT r s f = let
  enqueue b c m q = case m of
    Empty         -> q
    Singleton p v -> case r p of
      Just x  -> Q.insert x (Left (p, v), c) q
      Nothing -> q
    Branch n u    -> case s n b of
      Just x  -> Q.insert x (Right (b, u), c) q
      Nothing -> q
  go q = case Q.minViewWithKey q of
    Nothing                           -> Nothing
    Just ((a, (Left  (p, v), c)), _ ) -> Just (c . maybeSingletonT p <$> f a p v)
    Just ((_, (Right (b, u), c)), q') -> go $ ifoldr (\i -> enqueue (subbox b i) (c . makeBranch . flip (insert i) u)) q' u
  start b m = go $ enqueue b id m Q.empty
  in start

findBestT :: (Coordinate b p i,
              Maplike i m,
              Measure a p,
              Ord o)
          => (p -> Maybe o)
          -> (a -> b -> Maybe o)
          -> b
          -> SpaceTree a p i b m v
          -> Maybe o
findBestT r s b = fmap getConst . alterBestWithKeyFT r s (\a _ _ -> Const a) b


alterMinWithKeyFT :: (Coordinate b p i,
                      Maplike i m,
                      Measure a p,
                      Functor f)
                  => (p -> v -> f (Maybe v))
                  -> b
                  -> SpaceTree a p i b m v
                  -> Maybe (f (SpaceTree a p i b m v))
alterMinWithKeyFT f = alterBestWithKeyFT Just (const (Just . leastPoint)) (const f)


alterMaxWithKeyFT :: (Coordinate b p i,
                      Maplike i m,
                      Measure a p,
                      Functor f)
                  => (p -> v -> f (Maybe v))
                  -> b
                  -> SpaceTree a p i b m v
                  -> Maybe (f (SpaceTree a p i b m v))
alterMaxWithKeyFT f = alterBestWithKeyFT (Just . Down) (const (Just . Down . greatestPoint)) (const f)


alterAnyWithKeyFT :: (Coordinate b p i,
                      Maplike i m,
                      Measure a p,
                      Functor f)
                  => (p -> v -> f (Maybe v))
                  -> SpaceTree a p i b m v
                  -> Maybe (f (SpaceTree a p i b m v))
alterAnyWithKeyFT _ Empty           = Nothing
alterAnyWithKeyFT f (Singleton p v) = Just (maybeSingletonT p <$> f p v)
alterAnyWithKeyFT f (Branch _ u)    = let
  h (Just m) = m
  h Nothing  = error "alterAnyWithKeyFT: unexpected Nothing"
  in fmap makeBranch <$> alterAnyF (fmap nonNullT . h . alterAnyWithKeyFT f) u


foldlBestWithKeyT :: (Coordinate b p i,
                      Maplike i m,
                      Ord o)
                  => (p -> Maybe o)
                  -> (a -> b -> Maybe o)
                  -> (c -> p -> v -> c)
                  -> c
                  -> b
                  -> SpaceTree a p i b m v
                  -> c
foldlBestWithKeyT r s f = let
  enqueue b m q = case m of
    Empty         -> q
    Singleton p v -> case r p of
      Just x  -> Q.insert x (Left (p, v)) q
      Nothing -> q
    Branch n u    -> case s n b of
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
                      Ord o)
                  => (p -> Maybe o)
                  -> (a -> b -> Maybe o)
                  -> (p -> v -> c -> c)
                  -> c
                  -> b
                  -> SpaceTree a p i b m v
                  -> c
foldrBestWithKeyT r s f z = let
  enqueue b m q = case m of
    Empty         -> q
    Singleton p v -> case r p of
      Just x  -> Q.insert x (Left (p, v)) q
      Nothing -> q
    Branch n u    -> case s n b of
      Just x  -> Q.insert x (Right (b, u)) q
      Nothing -> q
  go q = case Q.minView q of
    Nothing                 -> z
    Just (Left (p, v), q')  -> f p v $ go q'
    Just (Right (b, u), q') -> go $ ifoldr (enqueue . subbox b) q' u
  start b m = go $ enqueue b m Q.empty
  in start


instance (Eq a,
          Eq p,
          forall x. Eq x => Eq (m x),
          Eq v)
      => Eq (SpaceTree a p i b m v) where
  Empty           == Empty           = True
  Singleton p1 v1 == Singleton p2 v2 = p1 == p2 && v1 == v2
  Branch n1 m1    == Branch n2 m2    = n1 == n2 && m1 == m2 -- testing the invariant is usually faster
  _               == _               = False


instance Functor m => FunctorWithIndex p (SpaceTree a p i b m) where
  imap _ Empty = Empty
  imap f (Singleton p v) = Singleton p $ f p v
  imap f (Branch n u) = Branch n $ fmap (imap f) u

instance Foldable m => Foldable (SpaceTree a p i b m) where
  foldr _ z Empty = z
  foldr f z (Singleton _ a) = f a z
  foldr f z (Branch _ u) = foldr (flip (foldr f)) z u
  foldMap _ Empty = mempty
  foldMap f (Singleton _ a) = f a
  foldMap f (Branch _ u) = foldMap (foldMap f) u

instance Foldable m => FoldableWithIndex p (SpaceTree a p i b m) where
  ifoldr _ z Empty = z
  ifoldr f z (Singleton p a) = f p a z
  ifoldr f z (Branch _ u) = foldr (flip (ifoldr f)) z u
  ifoldMap _ Empty = mempty
  ifoldMap f (Singleton p a) = f p a
  ifoldMap f (Branch _ u) = foldMap (ifoldMap f) u

instance Traversable m => Traversable (SpaceTree a p i b m) where
  traverse _ Empty = pure Empty
  traverse f (Singleton p a) = Singleton p <$> f a
  traverse f (Branch n u) = Branch n <$> traverse (traverse f) u

instance Traversable m => TraversableWithIndex p (SpaceTree a p i b m) where
  itraverse _ Empty = pure Empty
  itraverse f (Singleton p a) = Singleton p <$> f p a
  itraverse f (Branch n u) = Branch n <$> traverse (itraverse f) u

instance (Maplike i m, Measure a p) => Filterable (SpaceTree a p i b m) where
  mapMaybe _ Empty = Empty
  mapMaybe f (Singleton p a) = maybeSingletonT p (f a)
  mapMaybe f (Branch _ u) = makeBranch $ mapMaybe (nonNullT . mapMaybe f) u

instance (Maplike i m, Measure a p) => FilterableWithIndex p (SpaceTree a p i b m) where
  imapMaybe _ Empty = Empty
  imapMaybe f (Singleton p a) = maybeSingletonT p (f p a)
  imapMaybe f (Branch _ u) = makeBranch $ mapMaybe (nonNullT . imapMaybe f) u

instance (Maplike i m, Measure a p) => Witherable (SpaceTree a p i b m) where
  wither _ Empty = pure Empty
  wither f (Singleton p a) = maybeSingletonT p <$> f a
  wither f (Branch _ u) = makeBranch <$> wither (fmap nonNullT . wither f) u

instance (Maplike i m, Measure a p) => WitherableWithIndex p (SpaceTree a p i b m) where
  iwither _ Empty = pure Empty
  iwither f (Singleton p a) = maybeSingletonT p <$> f p a
  iwither f (Branch _ u) = makeBranch <$> wither (fmap nonNullT . iwither f) u


-- | A shortcutting map
transformT :: (Coordinate b p i,
               Maplike i m,
               Measure a p,
               Applicative f)
           => (p -> u -> f (Maybe v))
           -> (b -> Maybe (WhenMissing f p u v))
           -> b
           -> SpaceTree a p i b m u
           -> f (SpaceTree a p i b m v)
transformT f g = let
  go _ Empty = pure Empty
  go _ (Singleton p u) = maybeSingletonT p <$> f p u
  go b m@(Branch _ r) = case g b of
    Just t  -> runWhenMissing t m
    Nothing -> makeBranch <$> iwither (\i -> fmap nonNullT . go (subbox b i)) r
  in go


fastFilter :: (Coordinate b p i,
               Maplike i m,
               Measure a p)
           => (p -> Bool)
           -> (a -> b -> Maybe Bool)
           -> b
           -> SpaceTree a p i b m v
           -> SpaceTree a p i b m v
fastFilter f g = let
  go _ Empty = Empty
  go _ s@(Singleton p _) = if f p then s else Empty
  go b s@(Branch n u) = case g n b of
    Just True  -> s
    Just False -> Empty
    Nothing    -> makeBranch $ imapMaybe (\i -> nonNullT . go (subbox b i)) u
  in go

-- | The bits for which the predicates are true, followed by those for which they are false
fastPartition :: (Coordinate b p i,
                  Maplike i m,
                  Measure a p)
              => (p -> Bool)
              -> (a -> b -> Maybe Bool)
              -> b
              -> SpaceTree a p i b m v
              -> (SpaceTree a p i b m v, SpaceTree a p i b m v)
fastPartition f g = let
  complexToPair (x :+ y) = (x, y)
  go _ Empty = Empty :+ Empty
  go _ s@(Singleton p _) = if f p
    then s :+ Empty
    else Empty :+ s
  go b s@(Branch n u) = case g n b of
    Just True  -> s :+ Empty
    Just False -> Empty :+ s
    Nothing    -> makeBranch <$> iwither (\i -> fmap nonNullT . go (subbox b i)) u
  start b = complexToPair . go b
  in start

fastFilterA :: (Coordinate b p i,
                Maplike i m,
                Monad f,
                Measure a p)
            => (p -> f Bool)
            -> (a -> b -> f (Maybe Bool))
            -> b
            -> SpaceTree a p i b m v
            -> f (SpaceTree a p i b m v)
fastFilterA f g = let
  go _ Empty = pure Empty
  go _ s@(Singleton p _) = let
    h z = if z then s else Empty
    in h <$> f p
  go b s@(Branch n u) = let
    h (Just True)  = pure s
    h (Just False) = pure Empty
    h Nothing      = makeBranch <$> iwither (\i -> fmap nonNullT . go (subbox b i)) u
    in h =<< g n b
  in go


extent :: (Coordinate b p i, Foldable m) => SpaceTree a p i b m v -> Maybe b
extent = ifoldMap (\p _ -> Just (pointBox p))


data Ingredient a p i b m v = Point p v
                            | Chunk b a (m (SpaceTree a p i b m v))

instance Functor m => Functor (Ingredient a p i b m) where
  fmap f (Point p v) = Point p (f v)
  fmap f (Chunk b n m) = Chunk b n (fmap f <$> m)

instance Functor m => FunctorWithIndex p (Ingredient a p i b m) where
  imap f (Point p v) = Point p (f p v)
  imap f (Chunk b n m) = Chunk b n (imap f <$> m)

instance Foldable m => Foldable (Ingredient a p i b m) where
  foldMap f (Point _ v) = f v
  foldMap f (Chunk _ _ m) = foldMap (foldMap f) m

instance Foldable m => FoldableWithIndex p (Ingredient a p i b m) where
  ifoldMap f (Point p v) = f p v
  ifoldMap f (Chunk _ _ m) = foldMap (ifoldMap f) m

instance Traversable m => Traversable (Ingredient a p i b m) where
  traverse f (Point p v) = Point p <$> f v
  traverse f (Chunk b n m) = Chunk b n <$> traverse (traverse f) m

instance Traversable m => TraversableWithIndex p (Ingredient a p i b m) where
  itraverse f (Point p v) = Point p <$> f p v
  itraverse f (Chunk b n m) = Chunk b n <$> traverse (itraverse f) m


-- | Ingredients are bits of spacetree. All algorithms preserve the
-- property that the components are spatially disjoint.
newtype Ingredients a p i b m v = Ingredients {
  ingredients :: Vector (Ingredient a p i b m v)
} deriving (Functor, Semigroup, Monoid)

instance Functor m => FunctorWithIndex p (Ingredients a p i b m) where
  imap f (Ingredients a) = Ingredients (imap f <$> a)

instance Foldable m => Foldable (Ingredients a p i b m) where
  foldMap f (Ingredients a) = foldMap (foldMap f) a

instance Foldable m => FoldableWithIndex p (Ingredients a p i b m) where
  ifoldMap f (Ingredients a) = foldMap (ifoldMap f) a

instance Traversable m => Traversable (Ingredients a p i b m) where
  traverse f (Ingredients a) = Ingredients <$> traverse (traverse f) a

instance Traversable m => TraversableWithIndex p (Ingredients a p i b m) where
  itraverse f (Ingredients a) = Ingredients <$> traverse (itraverse f) a

instance (Maplike i m,
          Measure a p)
      => Filterable (Ingredients a p i b m) where
  mapMaybe f (Ingredients a) = let
    g (Point p u) = case f u of
      Just v  -> pure (Point p v)
      Nothing -> V.empty
    g (Chunk b _ m) = makeIngredients' b . makeBranch $ mapMaybe (nonNullT . mapMaybe f) m
    in Ingredients (g =<< a)

instance (Maplike i m,
          Measure a p)
      => FilterableWithIndex p (Ingredients a p i b m) where
  imapMaybe f (Ingredients a) = let
    g (Point p u) = case f p u of
      Just v  -> pure (Point p v)
      Nothing -> V.empty
    g (Chunk b _ m) = makeIngredients' b . makeBranch $ mapMaybe (nonNullT . imapMaybe f) m
    in Ingredients (g =<< a)

instance (Maplike i m,
          Measure a p)
      => Witherable (Ingredients a p i b m) where
  wither f (Ingredients a) = let
    g (Point p u) = let
      h (Just v) = pure (Point p v)
      h Nothing  = V.empty
      in h <$> f u
    g (Chunk b _ m) = makeIngredients' b . makeBranch <$> wither (fmap nonNullT . wither f) m
    in Ingredients . join <$> traverse g a

instance (Maplike i m,
          Measure a p)
      => WitherableWithIndex p (Ingredients a p i b m) where
  iwither f (Ingredients a) = let
    g (Point p u) = let
      h (Just v) = pure (Point p v)
      h Nothing  = V.empty
      in h <$> f p u
    g (Chunk b _ m) = makeIngredients' b . makeBranch <$> wither (fmap nonNullT . iwither f) m
    in Ingredients . join <$> traverse g a

pureI :: Ingredient a p i b m v -> Ingredients a p i b m v
pureI = Ingredients . pure

emptyI :: Ingredients a p i b m v
emptyI = Ingredients V.empty

makeIngredient :: b -> SpaceTree a p i b m v -> Maybe (Ingredient a p i b m v)
makeIngredient _ Empty           = Nothing
makeIngredient _ (Singleton p v) = Just $ Point p v
makeIngredient b (Branch n m)    = Just $ Chunk b n m

makeIngredients' :: b -> SpaceTree a p i b m v -> Vector (Ingredient a p i b m v)
makeIngredients' _ Empty           = V.empty
makeIngredients' _ (Singleton p v) = pure $ Point p v
makeIngredients' b (Branch n m)    = pure $ Chunk b n m

makeIngredients :: b
                -> SpaceTree a p i b m v
                -> Ingredients a p i b m v
makeIngredients b = Ingredients . makeIngredients' b

partitionIngredients :: (Coordinate b p i,
                         Maplike i m)
                     => b
                     -> Ingredients a p i b m v
                     -> (Ingredients a p i b m v, Ingredients a p i b m v)
partitionIngredients b = let
  f a@(Point p _)
    | containsPoint b p = (pureI a, emptyI)
    | otherwise         = (emptyI, pureI a)
  f c@(Chunk b' _ m)
    | isSubset b' b = (pureI c, emptyI)
    | disjoint b' b = (emptyI, pureI c)
    | otherwise     = let
        g i = foldMap f . ingredients . makeIngredients (subbox b' i)
        in ifoldMap g m
  in foldMap f . ingredients

pointFromIngredients :: (Coordinate b p i,
                         Maplike i m,
                         Measure a p)
                     => Ingredients a p i b m v
                     -> Maybe p
pointFromIngredients (Ingredients v) = let
  f (Point p _) = p
  f (Chunk _ n m) = case alterAnyWithKeyFT (\p _ -> Const p) (Branch n m) of
    Just (Const p) -> p
    Nothing        -> error "pointFromIngredients: should find a point in a chunk"
  in f <$> (v V.!? 0)

subdivideIngredients :: (Coordinate b p i,
                         Maplike i m,
                         Measure a p)
                     => b
                     -> Ingredients a p i b m v
                     -> m (b, Ingredients a p i b m v)
subdivideIngredients b = let
  go a = case pointFromIngredients a of
    Nothing -> []
    Just p  -> let
      (i, b') = narrow b p
      (r, a') = partitionIngredients b' a
      in (i,(b', r)):go a'
  in fromFold . go

classifyIngredients :: Ingredients a p i b m v -> Classified p v
classifyIngredients (Ingredients a) = case V.uncons a of
  Nothing               -> Zero
  Just (Point p v, a')
    | V.null a'         -> One p v
    | otherwise         -> Many
  Just (Chunk _ _ _, _) -> Many

assembleIngredients :: (Coordinate b p i,
                        Maplike i m,
                        Measure a p)
                    => b
                    -> Ingredients a p i b m v
                    -> SpaceTree a p i b m v
assembleIngredients b a = case classifyIngredients a of
  Zero    -> Empty
  One p v -> Singleton p v
  Many    -> makeBranchUnsafe . fmap (uncurry assembleIngredients) $ subdivideIngredients b a


-- | Change bounding box, assume (and do not check) that all points
-- are in the new bounding box
reboundT :: (Coordinate b p i,
             Maplike i m,
             Measure a p)
         => b
         -> b
         -> SpaceTree a p i b m v
         -> SpaceTree a p i b m v
reboundT old new = assembleIngredients new . makeIngredients old



-- | An abstraction, unlikely to be useful to the end user: its aim is
-- to stop us having to write four different merge algorithms based on
-- which of the arguments need resizing.
class Mergee c a p i b m | c -> a, c -> p, c -> i, c -> b, c -> m where
  classifyMergee :: c v -> Classified p v
  subdivideMergee :: b -> c v -> m (c v)
  runWhenMissingMergee :: Applicative f => WhenMissing f p u v -> b -> c u -> f (SpaceTree a p i b m v)
  simpleRunWhenMissingMergee :: SimpleWhenMissing p u v -> b -> c u -> SpaceTree a p i b m v

instance (Maplike i m,
          Measure a p)
      => Mergee (SpaceTree a p i b m) a p i b m where
  classifyMergee = classifyT
  subdivideMergee _ Empty           = error "subdivideMergee: should not be called on Empty"
  subdivideMergee _ (Singleton _ _) = error "subdivideMergee: should not be called on Singleton"
  subdivideMergee _ (Branch _ m)    = m
  runWhenMissingMergee t _ = runWhenMissing t
  simpleRunWhenMissingMergee t _ = runIdentity . runWhenMissing t

instance (Maplike i m,
          Measure a p,
          Coordinate b p i)
      => Mergee (Ingredients a p i b m) a p i b m where
  classifyMergee = classifyIngredients
  subdivideMergee b = let
    go a = case pointFromIngredients a of
      Nothing -> []
      Just p  -> let
        (i, b') = narrow b p
        (r, a') = partitionIngredients b' a
        in (i,r):go a'
    in fromFold . go
  runWhenMissingMergee t b = let
    f (Point p v) = fmap (Point p) <$> missingKey t p v
    f (Chunk a n m) = makeIngredient a <$> runWhenMissing t (Branch n m)
    in fmap (assembleIngredients b . Ingredients) . wither f . ingredients
  simpleRunWhenMissingMergee t b = let
    f (Point p v) = Point p <$> simpleMissingKey t p v
    f (Chunk a n m) = makeIngredient a $ runSimpleWhenMissing t (Branch n m)
    in assembleIngredients b . Ingredients . mapMaybe f . ingredients


-- | A very general merge algorithm, to provide support for neither,
-- either or both arguments to need resizing.
imergeT :: (Coordinate b p i,
            Maplike i m,
            Measure a p,
            Mergee c a p i b m,
            Mergee d a p i b m)
         => SimpleWhenMissing p u w
         -> SimpleWhenMissing p v w
         -> SimpleWhenMatched p u v w
         -> b
         -> c u
         -> d v
         -> SpaceTree a p i b m w
imergeT onL onR onB = let

  go b x y = case (classifyMergee x, classifyMergee y) of
    (Zero, Zero) -> Empty
    (One p u, Zero) -> maybeSingletonT p $ simpleMissingKey onL p u
    (Zero, One q v) -> maybeSingletonT q $ simpleMissingKey onR q v
    (One p u, One q v)
      | p == q    -> maybeSingletonT p $ simpleMatchedKey onB p u v
      | otherwise -> maybeDoubleton2 b p q (simpleMissingKey onL p u) (simpleMissingKey onR q v)
    (Many, Zero) -> simpleRunWhenMissingMergee onL b x
    (Zero, Many) -> simpleRunWhenMissingMergee onR b y
    (Many, Many) -> let
      onl = mapMaybeMissing (\i u -> nonNullT $ simpleRunWhenMissingMergee onL (subbox b i) u)
      onr = mapMaybeMissing (\i v -> nonNullT $ simpleRunWhenMissingMergee onR (subbox b i) v)
      onb i u v = nonNullT $ go (subbox b i) u v
      pair = imerge onl onr (zipWithMaybeMatched onb)
      in makeBranch $ pair (subdivideMergee b x) (subdivideMergee b y)
    (Many, One q _) -> let
      f i z = let
        b' = subbox b i
        in if containsPoint b' q
           then go b' z y
           else simpleRunWhenMissingMergee onL b' z
      in makeBranch $ imap f (subdivideMergee b x)
    (One p _, Many) -> let
      f i z = let
        b' = subbox b i
        in if containsPoint b' p
           then go b' x z
           else simpleRunWhenMissingMergee onR b' z
      in makeBranch $ imap f (subdivideMergee b y)
  in go


-- | A very general applicative-valued merge algorithm, to provide
-- support for neither, either or both arguments to need resizing.
imergeAT :: (Applicative f,
             Coordinate b p i,
             Maplike i m,
             Measure a p,
             Mergee c a p i b m,
             Mergee d a p i b m)
         => WhenMissing f p u w
         -> WhenMissing f p v w
         -> WhenMatched f p u v w
         -> b
         -> c u
         -> d v
         -> f (SpaceTree a p i b m w)
imergeAT onL onR onB = let

  go b x y = case (classifyMergee x, classifyMergee y) of
    (Zero, Zero) -> pure Empty
    (One p u, Zero) -> maybeSingletonT p <$> missingKey onL p u
    (Zero, One q v) -> maybeSingletonT q <$> missingKey onR q v
    (One p u, One q v)
      | p == q    -> maybeSingletonT p <$> matchedKey onB p u v
      | otherwise -> liftA2 (maybeDoubleton2 b p q) (missingKey onL p u) (missingKey onR q v)
    (Many, Zero) -> runWhenMissingMergee onL b x
    (Zero, Many) -> runWhenMissingMergee onR b y
    (Many, Many) -> let
      onl = traverseMaybeMissing (\i u -> nonNullT <$> runWhenMissingMergee onL (subbox b i) u)
      onr = traverseMaybeMissing (\i v -> nonNullT <$> runWhenMissingMergee onR (subbox b i) v)
      onb i u v = nonNullT <$> go (subbox b i) u v
      pair = imergeA onl onr (WhenMatched onb)
      in makeBranch <$> pair (subdivideMergee b x) (subdivideMergee b y)
    (Many, One q _) -> let
      f i z = let
        b' = subbox b i
        in if containsPoint b' q
           then go b' z y
           else runWhenMissingMergee onL b' z
      in makeBranch <$> itraverse f (subdivideMergee b x)
    (One p _, Many) -> let
      f i z = let
        b' = subbox b i
        in if containsPoint b' p
           then go b' x z
           else runWhenMissingMergee onR b' z
      in makeBranch <$> itraverse f (subdivideMergee b y)
  in go
