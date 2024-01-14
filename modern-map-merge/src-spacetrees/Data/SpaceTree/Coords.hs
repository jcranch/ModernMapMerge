{-# LANGUAGE
      FlexibleInstances,
      FunctionalDependencies,
      MultiParamTypeClasses
  #-}

module Data.SpaceTree.Coords where


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

isSuperset :: Coordinate b p i => b -> b -> Bool
isSuperset = flip isSubset


-- | Coordinates with a standard outer box
class (Coordinate b p i) => DefaultCoordinate b p i where
  outer :: b


data Ival n = Ival {
  leftEnd :: n,
  rightEnd :: n
} deriving (Eq, Ord, Read, Show)

midPoint :: Fractional n => Ival n -> n
midPoint (Ival a b) = (a+b)/2

instance Ord n => Semigroup (Ival n) where
  Ival a b <> Ival c d = Ival (min a c) (max b d)

instance (Fractional n, Ord n) => Coordinate (Ival n) n Bool where

  narrow i@(Ival a b) c = let
    m = midPoint i
    in if c < m
       then (False, Ival a m)
       else (True,  Ival m b)

  subbox i@(Ival a _) False = Ival a (midPoint i)
  subbox i@(Ival _ b) True  = Ival (midPoint i) b

  pointBox a = Ival a a

  disjoint (Ival a b) (Ival c d) = b < c || d < a

  isSubset (Ival a b) (Ival c d) = c >= a && d <= b

  containsPoint (Ival a b) c = case compare a c of
    GT -> False
    EQ -> True
    LT -> c < b

  leastPoint    (Ival a _) = a
  greatestPoint (Ival _ b) = b



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

