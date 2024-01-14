{-# LANGUAGE
      FunctionalDependencies,
      MultiParamTypeClasses
  #-}

module Data.SpaceTree.Coords where


-- | A class of "boxes" `b` which contain points `p`, and are
-- comprised of subboxes indexed by `i`. The semigroup structure
-- should yield a box (preferably the smallest possible such)
-- containing both arguments.
class (Eq b, Semigroup b, Eq p, Eq i)
    => Coordinate b p i | b -> i, b -> p where

  -- | Which subbox is a point in?
  narrow :: b -> p -> (i,b)

  -- | Which subbox is given by an index?
  subbox :: b -> i -> b

  -- | A one-point box
  pointBox :: p -> b

  -- | Do boxes overlap?
  overlaps :: b -> b -> Bool

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
