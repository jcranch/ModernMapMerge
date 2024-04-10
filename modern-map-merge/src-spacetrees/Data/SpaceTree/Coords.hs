{-# LANGUAGE
      FlexibleInstances,
      FunctionalDependencies,
      MultiParamTypeClasses,
      RankNTypes
  #-}

module Data.SpaceTree.Coords where

import Data.Ord (Down(..))


-- | A class of "boxes" `b` which contain points `p`, and are
-- comprised of subboxes indexed by `i`. The semigroup structure
-- should yield a box (preferably the smallest possible such)
-- containing both arguments.
class (Eq b, Semigroup b, Ord p, Eq i)
    => Coordinate b p i | b -> i, b -> p where

  -- | Which subbox is a point in?
  narrow :: b -> p -> (i,b)

  -- | Which subbox is given by an index?
  subbox :: b -> i -> b

  -- | A one-point box
  pointBox :: p -> b

  -- | Do boxes overlap?
  disjoint :: b -> b -> Bool

  -- | Does the second box entirely contain the first?
  isSubset :: b -> b -> Bool

  -- | Is the point in the box?
  containsPoint :: b -> p -> Bool
  containsPoint b = isSuperset b . pointBox

  -- | Not very interesting in general, but needed for `alterMinWithKeyF`
  leastPoint :: b -> p

  -- | Not very interesting in general, but needed for `alterMaxWithKeyF`
  greatestPoint :: b -> p

  -- | Used to narrow down bounding boxes
  findExtent :: (forall v. Ord v => (p -> v) -> (b -> v) -> x -> Maybe v) -> x -> Maybe b


isSuperset :: Coordinate b p i => b -> b -> Bool
isSuperset = flip isSubset


-- | Coordinates with a standard outer box
class (Coordinate b p i) => DefaultCoordinate b p i where
  outer :: b


data Ival n = Ival {
  leftEnd :: n,
  closedLeft :: Bool,
  rightEnd :: n,
  closedRight :: Bool
} deriving (Eq, Ord, Read, Show)

midPoint :: Fractional n => Ival n -> n
midPoint (Ival a _ b _) = (a+b)/2

instance Ord n => Semigroup (Ival n) where
  Ival a p b q <> Ival c r d s = let
    (e,t) = case compare a c of
      LT -> (a, p)
      EQ -> (a, p || r)
      GT -> (c, r)
    (f,u) = case compare b d of
      LT -> (d, s)
      EQ -> (d, q || s)
      GT -> (b, q)
    in Ival e t f u

instance (Fractional n, Ord n) => Coordinate (Ival n) n Bool where

  narrow i@(Ival a p b q) c = let
    m = midPoint i
    in if c < m
       then (False, Ival a p m True)
       else (True,  Ival m False b q)

  subbox i@(Ival a p _ _) False = Ival a p (midPoint i) True
  subbox i@(Ival _ _ b q) True  = Ival (midPoint i) False b q

  pointBox a = Ival a True a True

  disjoint (Ival a p b q) (Ival c r d s) = case compare b c of
    LT -> True
    EQ -> q && r
    GT -> case compare d a of
      LT -> True
      EQ -> p && s
      GT -> False

  isSubset (Ival a p b q) (Ival c r d s) = let
    s1 = case compare a c of
      GT -> True
      EQ -> r || not p
      LT -> False
    s2 = case compare b d of
      LT -> True
      EQ -> s || not q
      GT -> False
    in s1 && s2

  containsPoint (Ival a p b q) c = case compare a c of
    GT -> False
    EQ -> p
    LT -> case compare c b of
      GT -> False
      EQ -> q
      LT -> True

  leastPoint    (Ival a _ _ _) = a
  greatestPoint (Ival _ _ b _) = b

  findExtent minimise x = do
    a <- minimise id leftEnd x
    b <- getDown <$> minimise Down (Down . rightEnd) x
    pure $ Ival a True b True


data Rectangle b c p q i j = Rectangle {
  rectanglex :: b,
  rectangley :: c
} deriving (Eq, Ord, Read, Show)

instance (Semigroup b, Semigroup c) => Semigroup (Rectangle b c p q i j) where
  Rectangle x1 y1 <> Rectangle x2 y2 = Rectangle (x1 <> x2) (y1 <> y2)

instance (Coordinate b p i,
          Coordinate c q j)
       => Coordinate (Rectangle b c p q i j) (p,q) (i,j) where

  narrow (Rectangle u v) (p, q) = let
    (i, u') = narrow u p
    (j, v') = narrow v q
    in ((i, j), Rectangle u' v')

  subbox (Rectangle u v) (i, j) = Rectangle (subbox u i) (subbox v j)

  pointBox (p,q) = Rectangle (pointBox p) (pointBox q)

  disjoint (Rectangle u v) (Rectangle u' v') = disjoint u u' || disjoint v v'

  isSubset (Rectangle u v) (Rectangle u' v') = isSubset u u' && isSubset v v'

  leastPoint    (Rectangle u v) = (leastPoint u, leastPoint v)
  greatestPoint (Rectangle u v) = (greatestPoint u, greatestPoint v)

  findExtent f x = do
    u <- findExtent (\g h -> f (g . fst) (h . rectanglex)) x
    v <- findExtent (\g h -> f (g . snd) (h . rectangley)) x
    pure $ Rectangle u v


-- | The common case of a homogeneous rectangle
data Rectangle' b p i = Rectangle' {
  rectanglex' :: b,
  rectangley' :: b
} deriving (Eq, Ord, Read, Show)

instance (Semigroup b) => Semigroup (Rectangle' b p i) where
  Rectangle' x1 y1 <> Rectangle' x2 y2 = Rectangle' (x1 <> x2) (y1 <> y2)

instance (Coordinate b p i)
       => Coordinate (Rectangle' b p i) (p,p) (i,i) where

  narrow (Rectangle' u v) (p, q) = let
    (i, u') = narrow u p
    (j, v') = narrow v q
    in ((i, j), Rectangle' u' v')

  subbox (Rectangle' u v) (i, j) = Rectangle' (subbox u i) (subbox v j)

  pointBox (p,q) = Rectangle' (pointBox p) (pointBox q)

  disjoint (Rectangle' u v) (Rectangle' u' v') = disjoint u u' || disjoint v v'

  isSubset (Rectangle' u v) (Rectangle' u' v') = isSubset u u' && isSubset v v'

  leastPoint    (Rectangle' u v) = (leastPoint u, leastPoint v)
  greatestPoint (Rectangle' u v) = (greatestPoint u, greatestPoint v)

  findExtent f x = do
    u <- findExtent (\g h -> f (g . fst) (h . rectanglex')) x
    v <- findExtent (\g h -> f (g . snd) (h . rectangley')) x
    pure $ Rectangle' u v


data Cuboid b c d p q r i j k = Cuboid {
  cuboidx :: b,
  cuboidy :: c,
  cuboidz :: d
} deriving (Eq, Ord, Read, Show)

instance (Semigroup b, Semigroup c, Semigroup d) => Semigroup (Cuboid b c d p q r i j k) where
  Cuboid x1 y1 z1 <> Cuboid x2 y2 z2 = Cuboid (x1 <> x2) (y1 <> y2) (z1 <> z2)

instance (Coordinate b p i,
          Coordinate c q j,
          Coordinate d r k)
       => Coordinate (Cuboid b c d p q r i j k) (p,q,r) (i,j,k) where

  narrow (Cuboid u v w) (p, q, r) = let
    (i, u') = narrow u p
    (j, v') = narrow v q
    (k, w') = narrow w r
    in ((i, j, k), Cuboid u' v' w')

  subbox (Cuboid u v w) (i, j, k) = Cuboid (subbox u i) (subbox v j) (subbox w k)

  pointBox (p,q,r) = Cuboid (pointBox p) (pointBox q) (pointBox r)

  disjoint (Cuboid u v w) (Cuboid u' v' w') = disjoint u u' || disjoint v v' || disjoint w w'

  isSubset (Cuboid u v w) (Cuboid u' v' w') = isSubset u u' && isSubset v v' && isSubset w w'

  leastPoint    (Cuboid u v w) = (leastPoint u, leastPoint v, leastPoint w)
  greatestPoint (Cuboid u v w) = (greatestPoint u, greatestPoint v, greatestPoint w)

  findExtent f x = do
    u <- findExtent (\g h -> f (g . (\(a,_,_) -> a)) (h . cuboidx)) x
    v <- findExtent (\g h -> f (g . (\(_,a,_) -> a)) (h . cuboidy)) x
    w <- findExtent (\g h -> f (g . (\(_,_,a) -> a)) (h . cuboidz)) x
    pure $ Cuboid u v w


data Cuboid' b p i = Cuboid' {
  cuboidx' :: b,
  cuboidy' :: b,
  cuboidz' :: b
} deriving (Eq, Ord, Read, Show)

instance (Semigroup b) => Semigroup (Cuboid' b p i) where
  Cuboid' x1 y1 z1 <> Cuboid' x2 y2 z2 = Cuboid' (x1 <> x2) (y1 <> y2) (z1 <> z2)

instance (Coordinate b p i)
       => Coordinate (Cuboid' b p i) (p,p,p) (i,i,i) where

  narrow (Cuboid' u v w) (p, q, r) = let
    (i, u') = narrow u p
    (j, v') = narrow v q
    (k, w') = narrow w r
    in ((i, j, k), Cuboid' u' v' w')

  subbox (Cuboid' u v w) (i, j, k) = Cuboid' (subbox u i) (subbox v j) (subbox w k)

  pointBox (p,q,r) = Cuboid' (pointBox p) (pointBox q) (pointBox r)

  disjoint (Cuboid' u v w) (Cuboid' u' v' w') = disjoint u u' || disjoint v v' || disjoint w w'

  isSubset (Cuboid' u v w) (Cuboid' u' v' w') = isSubset u u' && isSubset v v' && isSubset w w'

  leastPoint    (Cuboid' u v w) = (leastPoint u, leastPoint v, leastPoint w)
  greatestPoint (Cuboid' u v w) = (greatestPoint u, greatestPoint v, greatestPoint w)

  findExtent f x = do
    u <- findExtent (\g h -> f (g . (\(a,_,_) -> a)) (h . cuboidx')) x
    v <- findExtent (\g h -> f (g . (\(_,a,_) -> a)) (h . cuboidy')) x
    w <- findExtent (\g h -> f (g . (\(_,_,a) -> a)) (h . cuboidz')) x
    pure $ Cuboid' u v w


-- | An object with spatial extent, indexed via a reference point.
--
-- Ideally that point should be in some sense central to the object.
class Blob a p | a -> p where
  reference :: a -> p
