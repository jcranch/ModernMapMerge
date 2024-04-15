{-# LANGUAGE
      FunctionalDependencies,
      MultiParamTypeClasses
  #-}

module Data.SpaceTree.Blob.Explicit where

import Data.Monoid (Sum(..))

import Data.Maplike
import Data.SpaceTree.Coords
import Data.SpaceTree.Explicit


-- | An object with spatial extent, indexed via a reference point.
--
-- Ideally that point should be in some sense central to the object.
class Measure a b => Blob a b p | a -> b, a -> p where
  reference :: a -> p


newtype BlobTree p i b m a n v = BlobTree {
  blobs :: SpaceTree (b, Sum Int) p i b m (n v)
}

classifyB :: BlobTree p i b m a n v -> Classified a v
classifyB = bindClassify _ _ . blobs

nonNullB :: BlobTree p i b m a n v -> Maybe (BlobTree p i b m a n v)
nonNullB (BlobTree t) = BlobTree <$> nonNullT t

sizeB :: BlobTree p i b m a n v -> Int
sizeB = _
