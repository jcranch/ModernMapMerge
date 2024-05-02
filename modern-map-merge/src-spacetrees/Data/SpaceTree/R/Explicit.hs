{-# LANGUAGE
      FunctionalDependencies,
      MultiParamTypeClasses
  #-}

-- | R-trees
--
-- https://en.wikipedia.org/wiki/R-tree
module Data.SpaceTree.R.Explicit where

import Data.Maplike
import Data.SpaceTree.Coords
import Data.SpaceTree.Explicit


-- | An object with spatial extent, indexed via a reference point.
--
-- Ideally that point should be in some sense central to the object.
class Measure a b => Blob a b p | a -> b, a -> p where
  reference :: a -> p


newtype BlobTree p i b m a n v = BlobTree {
  blobs :: SpaceTree (PairMeasure (BoxMeasure b p i) (Counting p) p) p i b m (n v)
}

classifyB :: (Maplike a n) => BlobTree p i b m a n v -> Classified a v
classifyB = bindClassify (const id) classify . classifyT . blobs

nonNullB :: BlobTree p i b m a n v -> Maybe (BlobTree p i b m a n v)
nonNullB (BlobTree t) = BlobTree <$> nonNullT t

sizeB :: (Coordinate b p i) => BlobTree p i b m a n v -> Int
sizeB = getCount . measureR . totalMeasure . blobs
