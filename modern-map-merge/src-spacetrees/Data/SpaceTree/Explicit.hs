{-# LANGUAGE
      DeriveFunctor,
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
import qualified Data.PQueue.Prio.Min as Q
import Data.Monoid (Sum(..))
import Data.Ord (Down(..))
import Data.Traversable.WithIndex
import Data.Vector (Vector)
import qualified Data.Vector as V
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

makeBranchUnsafe :: (Maplike i m) => m (SpaceTree p i b m v) -> SpaceTree p i b m v
makeBranchUnsafe m = Branch (getSum $ foldMap (Sum . sizeT) m) m

makeBranch :: (Maplike i m) => m (SpaceTree p i b m v) -> SpaceTree p i b m v
makeBranch m = case bindClassify (const id) classifyT (classify m) of
  Zero    -> Empty
  One p v -> Singleton p v
  Many    -> makeBranchUnsafe m

nullT :: SpaceTree p i b m v -> Bool
nullT Empty = True
nullT _     = False

maybeSingletonT :: p -> Maybe v -> SpaceTree p i b m v
maybeSingletonT _ Nothing  = Empty
maybeSingletonT p (Just v) = Singleton p v

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
                 Maplike i m)
             => p
             -> b
             -> Maybe v
             -> SpaceTree p i b m v
             -> SpaceTree p i b m v
maybeInsertT _ _ Nothing    = id
maybeInsertT p b v@(Just _) = runIdentity . alterFT (const $ Identity v) p b

-- | I think I need to be careful with strictness in this algorithm
alterBestWithKeyFT :: (Coordinate b p i,
                       Maplike i m,
                       Ord a,
                       Functor f)
                   => (p -> Maybe a)
                   -> (b -> Maybe a)
                   -> (a -> p -> v -> f (Maybe v))
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
  go q = case Q.minViewWithKey q of
    Nothing                           -> Nothing
    Just ((a, (Left  (p, v), c)), _ ) -> Just (c . maybeSingletonT p <$> f a p v)
    Just ((_, (Right (b, u), c)), q') -> go $ ifoldr (\i -> enqueue (subbox b i) (c . makeBranch . flip (insert i) u)) q' u
  start b m = go $ enqueue b id m Q.empty
  in start

findBestT :: (Coordinate b p i,
              Maplike i m,
              Ord a)
          => (p -> Maybe a)
          -> (b -> Maybe a)
          -> b
          -> SpaceTree p i b m v
          -> Maybe a
findBestT r s b = fmap getConst . alterBestWithKeyFT r s (\a _ _ -> Const a) b


alterMinWithKeyFT :: (Coordinate b p i,
                      Maplike i m,
                      Functor f)
                  => (p -> v -> f (Maybe v))
                  -> b
                  -> SpaceTree p i b m v
                  -> Maybe (f (SpaceTree p i b m v))
alterMinWithKeyFT f = alterBestWithKeyFT Just (Just . leastPoint) (const f)


alterMaxWithKeyFT :: (Coordinate b p i,
                      Maplike i m,
                      Functor f)
                  => (p -> v -> f (Maybe v))
                  -> b
                  -> SpaceTree p i b m v
                  -> Maybe (f (SpaceTree p i b m v))
alterMaxWithKeyFT f = alterBestWithKeyFT (Just . Down) (Just . Down . greatestPoint) (const f)


alterAnyWithKeyFT :: (Coordinate b p i,
                      Maplike i m,
                      Functor f)
                  => (p -> v -> f (Maybe v))
                  -> SpaceTree p i b m v
                  -> Maybe (f (SpaceTree p i b m v))
alterAnyWithKeyFT _ Empty           = Nothing
alterAnyWithKeyFT f (Singleton p v) = Just (maybeSingletonT p <$> f p v)
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
  mapMaybe f (Singleton p a) = maybeSingletonT p (f a)
  mapMaybe f (Branch _ u) = makeBranch $ mapMaybe (nonNullT . mapMaybe f) u

instance (Maplike i m) => FilterableWithIndex p (SpaceTree p i b m) where
  imapMaybe _ Empty = Empty
  imapMaybe f (Singleton p a) = maybeSingletonT p (f p a)
  imapMaybe f (Branch _ u) = makeBranch $ mapMaybe (nonNullT . imapMaybe f) u

instance (Maplike i m) => Witherable (SpaceTree p i b m) where
  wither _ Empty = pure Empty
  wither f (Singleton p a) = maybeSingletonT p <$> f a
  wither f (Branch _ u) = makeBranch <$> wither (fmap nonNullT . wither f) u

instance (Maplike i m) => WitherableWithIndex p (SpaceTree p i b m) where
  iwither _ Empty = pure Empty
  iwither f (Singleton p a) = maybeSingletonT p <$> f p a
  iwither f (Branch _ u) = makeBranch <$> wither (fmap nonNullT . iwither f) u


-- | A shortcutting map
transformT :: (Coordinate b p i,
               Maplike i m,
               Applicative f)
           => (p -> u -> f (Maybe v))
           -> (b -> Maybe (WhenMissing f p u v))
           -> b
           -> SpaceTree p i b m u
           -> f (SpaceTree p i b m v)
transformT f g = let
  go _ Empty = pure Empty
  go _ (Singleton p u) = maybeSingletonT p <$> f p u
  go b m@(Branch _ r) = case g b of
    Just t  -> runWhenMissing t m
    Nothing -> makeBranch <$> iwither (\i -> fmap nonNullT . go (subbox b i)) r
  in go


fastFilter :: (Coordinate b p i, Maplike i m) => (p -> Bool) -> (b -> Maybe Bool) -> b -> SpaceTree p i b m v -> SpaceTree p i b m v
fastFilter f g = let
  go _ Empty = Empty
  go _ s@(Singleton p _) = if f p then s else Empty
  go b s@(Branch _ u) = case g b of
    Just True  -> s
    Just False -> Empty
    Nothing    -> makeBranch $ imapMaybe (\i -> nonNullT . go (subbox b i)) u
  in go

-- | The bits for which the predicates are true, followed by those for which they are false
fastPartition :: (Coordinate b p i,
                  Maplike i m)
              => (p -> Bool)
              -> (b -> Maybe Bool)
              -> b
              -> SpaceTree p i b m v
              -> (SpaceTree p i b m v, SpaceTree p i b m v)
fastPartition f g = let
  complexToPair (x :+ y) = (x, y)
  go _ Empty = Empty :+ Empty
  go _ s@(Singleton p _) = if f p
    then s :+ Empty
    else Empty :+ s
  go b s@(Branch _ u) = case g b of
    Just True  -> s :+ Empty
    Just False -> Empty :+ s
    Nothing    -> makeBranch <$> iwither (\i -> fmap nonNullT . go (subbox b i)) u
  start b = complexToPair . go b
  in start

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


data Ingredient p i b m v = Point p v
                          | Chunk b Int (m (SpaceTree p i b m v))

instance Functor m => Functor (Ingredient p i b m) where
  fmap f (Point p v) = Point p (f v)
  fmap f (Chunk b n m) = Chunk b n (fmap f <$> m)

instance Functor m => FunctorWithIndex p (Ingredient p i b m) where
  imap f (Point p v) = Point p (f p v)
  imap f (Chunk b n m) = Chunk b n (imap f <$> m)

instance Foldable m => Foldable (Ingredient p i b m) where
  foldMap f (Point _ v) = f v
  foldMap f (Chunk _ _ m) = foldMap (foldMap f) m

instance Foldable m => FoldableWithIndex p (Ingredient p i b m) where
  ifoldMap f (Point p v) = f p v
  ifoldMap f (Chunk _ _ m) = foldMap (ifoldMap f) m

instance Traversable m => Traversable (Ingredient p i b m) where
  traverse f (Point p v) = Point p <$> f v
  traverse f (Chunk b n m) = Chunk b n <$> traverse (traverse f) m

instance Traversable m => TraversableWithIndex p (Ingredient p i b m) where
  itraverse f (Point p v) = Point p <$> f p v
  itraverse f (Chunk b n m) = Chunk b n <$> traverse (itraverse f) m


-- | Ingredients are bits of spacetree. All algorithms preserve the
-- property that the components are spatially disjoint.
newtype Ingredients p i b m v = Ingredients {
  ingredients :: Vector (Ingredient p i b m v)
} deriving (Functor, Semigroup, Monoid)

instance Functor m => FunctorWithIndex p (Ingredients p i b m) where
  imap f (Ingredients a) = Ingredients (imap f <$> a)

instance Foldable m => Foldable (Ingredients p i b m) where
  foldMap f (Ingredients a) = foldMap (foldMap f) a

instance Foldable m => FoldableWithIndex p (Ingredients p i b m) where
  ifoldMap f (Ingredients a) = foldMap (ifoldMap f) a

instance Traversable m => Traversable (Ingredients p i b m) where
  traverse f (Ingredients a) = Ingredients <$> traverse (traverse f) a

instance Traversable m => TraversableWithIndex p (Ingredients p i b m) where
  itraverse f (Ingredients a) = Ingredients <$> traverse (itraverse f) a

instance Maplike i m => Filterable (Ingredients p i b m) where
  mapMaybe f (Ingredients a) = let
    g (Point p u) = case f u of
      Just v  -> pure (Point p v)
      Nothing -> V.empty
    g (Chunk b _ m) = makeIngredients' b . makeBranch $ mapMaybe (nonNullT . mapMaybe f) m
    in Ingredients (g =<< a)

instance Maplike i m => FilterableWithIndex p (Ingredients p i b m) where
  imapMaybe f (Ingredients a) = let
    g (Point p u) = case f p u of
      Just v  -> pure (Point p v)
      Nothing -> V.empty
    g (Chunk b _ m) = makeIngredients' b . makeBranch $ mapMaybe (nonNullT . imapMaybe f) m
    in Ingredients (g =<< a)

instance Maplike i m => Witherable (Ingredients p i b m) where
  wither f (Ingredients a) = let
    g (Point p u) = let
      h (Just v) = pure (Point p v)
      h Nothing  = V.empty
      in h <$> f u
    g (Chunk b _ m) = makeIngredients' b . makeBranch <$> wither (fmap nonNullT . wither f) m
    in Ingredients . join <$> traverse g a

instance Maplike i m => WitherableWithIndex p (Ingredients p i b m) where
  iwither f (Ingredients a) = let
    g (Point p u) = let
      h (Just v) = pure (Point p v)
      h Nothing  = V.empty
      in h <$> f p u
    g (Chunk b _ m) = makeIngredients' b . makeBranch <$> wither (fmap nonNullT . iwither f) m
    in Ingredients . join <$> traverse g a

pureI :: Ingredient p i b m v -> Ingredients p i b m v
pureI = Ingredients . pure

emptyI :: Ingredients p i b m v
emptyI = Ingredients V.empty

makeIngredient :: b -> SpaceTree p i b m v -> Maybe (Ingredient p i b m v)
makeIngredient _ Empty           = Nothing
makeIngredient _ (Singleton p v) = Just $ Point p v
makeIngredient b (Branch n m)    = Just $ Chunk b n m

makeIngredients' :: b -> SpaceTree p i b m v -> Vector (Ingredient p i b m v)
makeIngredients' _ Empty           = V.empty
makeIngredients' _ (Singleton p v) = pure $ Point p v
makeIngredients' b (Branch n m)    = pure $ Chunk b n m

makeIngredients :: b
                -> SpaceTree p i b m v
                -> Ingredients p i b m v
makeIngredients b = Ingredients . makeIngredients' b

partitionIngredients :: (Coordinate b p i,
                         Maplike i m)
                     => b
                     -> Ingredients p i b m v
                     -> (Ingredients p i b m v, Ingredients p i b m v)
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
                         Maplike i m)
                     => Ingredients p i b m v
                     -> Maybe p
pointFromIngredients (Ingredients v) = let
  f (Point p _) = p
  f (Chunk _ n m) = case alterAnyWithKeyFT (\p _ -> Const p) (Branch n m) of
    Just (Const p) -> p
    Nothing        -> error "pointFromIngredients: should find a point in a chunk"
  in f <$> (v V.!? 0)

subdivideIngredients :: (Coordinate b p i,
                         Maplike i m)
                     => b
                     -> Ingredients p i b m v
                     -> m (b, Ingredients p i b m v)
subdivideIngredients b = let
  go a = case pointFromIngredients a of
    Nothing -> []
    Just p  -> let
      (i, b') = narrow b p
      (r, a') = partitionIngredients b' a
      in (i,(b', r)):go a'
  in fromFold . go

classifyIngredients :: Ingredients p i b m v -> Classified p v
classifyIngredients (Ingredients a) = case V.uncons a of
  Nothing               -> Zero
  Just (Point p v, a')
    | V.null a'         -> One p v
    | otherwise         -> Many
  Just (Chunk _ _ _, _) -> Many

assembleIngredients :: (Coordinate b p i,
                        Maplike i m)
                    => b
                    -> Ingredients p i b m v
                    -> SpaceTree p i b m v
assembleIngredients b a = case classifyIngredients a of
  Zero    -> Empty
  One p v -> Singleton p v
  Many    -> makeBranchUnsafe . fmap (uncurry assembleIngredients) $ subdivideIngredients b a


-- | Change bounding box, assume (and do not check) that all points
-- are in the new bounding box
reboundT :: (Coordinate b p i,
             Maplike i m)
         => b
         -> b
         -> SpaceTree p i b m v
         -> SpaceTree p i b m v
reboundT old new = assembleIngredients new . makeIngredients old



-- | An abstraction, unlikely to be useful to the end user: its aim is
-- to stop us having to write four different merge algorithms based on
-- which of the arguments need resizing.
class Mergee c p i b m | c -> p, c -> i, c -> b, c -> m where
  classifyMergee :: c v -> Classified p v
  subdivideMergee :: b -> c v -> m (c v)
  runWhenMissingMergee :: Applicative f => WhenMissing f p u v -> b -> c u -> f (SpaceTree p i b m v)

instance Maplike i m => Mergee (SpaceTree p i b m) p i b m where
  classifyMergee = classifyT
  subdivideMergee _ Empty           = error "subdivideMergee: should not be called on Empty"
  subdivideMergee _ (Singleton _ _) = error "subdivideMergee: should not be called on Singleton"
  subdivideMergee _ (Branch _ m)    = m
  runWhenMissingMergee t _ x = runWhenMissing t x

instance (Maplike i m, Coordinate b p i) => Mergee (Ingredients p i b m) p i b m where
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

-- | A very general merge algorithm, to provide support for neither,
-- either or both arguments to need resizing.
imergeAT :: (Applicative f,
             Coordinate b p i,
             Maplike i m,
             Mergee c p i b m,
             Mergee d p i b m)
         => WhenMissing f p u w
         -> WhenMissing f p v w
         -> WhenMatched f p u v w
         -> b
         -> c u
         -> d v
         -> f (SpaceTree p i b m w)
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

{-


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
    | p1 == p2                                 = maybeSingletonT p1 <$> matchedKey t p1 v1 v2
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


-- | A general merge function, where we may have to rebound either or
-- both of the maps.
imergeAT :: (Coordinate b p i,
             Maplike i m,
             Applicative f)
         => WhenMissing f p u w
         -> WhenMissing f p v w
         -> WhenMatched f p u v w
         -> Maybe b
         -> Maybe b
         -> b
         -> SpaceTree p i b m u
         -> SpaceTree p i b m v
         -> f (SpaceTree p i b m w)
imergeAT onL onR onB = let

  go Nothing _ _ m Empty = runWhenMissing onL m

  go _ Nothing _ Empty n = runWhenMissing onR n

  go (Just b) _ d m Empty = reboundT b d <$> runWhenMissing onL m

  go _ (Just c) d Empty n = reboundT c d <$> runWhenMissing onR n

  go _ _ d (Singleton p u) (Singleton q v)
    | p == q    = maybeSingletonT p <$> matchedKey onB p u v
    | otherwise = liftA2 (maybeDoubleton2 d p q) (missingKey onL p u) (missingKey onR q v)

  go Nothing Nothing d (Branch _ m) (Branch _ n) = let
    pair = let
      onl = utraverseMaybeMissing (fmap nonNullT . runWhenMissing onL)
      onr = utraverseMaybeMissing (fmap nonNullT . runWhenMissing onR)
      onb i u v = nonNullT <$> go Nothing Nothing (subbox d i) u v
      in imergeA onl onr (WhenMatched onb)
    in makeBranch <$> pair m n

  go (Just b) Nothing d r (Branch _ n) = let
    pair = let
      onl = utraverseMaybeMissing (fmap nonNullT . _)
      onr = utraverseMaybeMissing (fmap nonNullT . runWhenMissing onR)
      -- onb :: i -> (a, Ingredients p i b m u) -> SpaceTree p i b m v -> f (Maybe (SpaceTree p i b m w))
      onb _ (b', u) n' = nonNullT <$> _
      in imergeA onl onr (WhenMatched onb)
    in makeBranch <$> _ (subdivideIngredients d $ makeIngredients b r) n


  go _ Nothing d r@(Singleton p _) (Branch _ n) = let
    (i, _) = narrow d p
    in pair d (singleton i r) n
  go Nothing _ d (Branch _ m) s@(Singleton q _) = let
    (j, _) = narrow d q
    in pair d m (singleton j s)

  go _ (Just c) d (Singleton p u) n
    | containsPoint c p = let
        f q v
          | p == q    = matchedKey onB p u v
          | otherwise = missingKey onR q v
        g c'
          | containsPoint c' p = Nothing
          | otherwise          = Just onR
        in reboundT c d <$> transformT f g c n
    | otherwise = liftA2 (maybeInsertT p d) (missingKey onL p u) (reboundT c d <$> runWhenMissing onR n)
  go (Just b) _ d m (Singleton q v)
    | containsPoint b q = let
        f p u
          | p == q    = matchedKey onB p u v
          | otherwise = missingKey onL p u
        g b'
          | containsPoint b' q = Nothing
          | otherwise          = Just onL
        in reboundT b d <$> transformT f g b m
    | otherwise = liftA2 (maybeInsertT q d) (missingKey onR q v) (reboundT b d <$> runWhenMissing onL m)
  in go
-}
