{-# LANGUAGE
      FlexibleInstances,
      FunctionalDependencies,
      MultiParamTypeClasses
  #-}

-- | R-trees
--
-- https://en.wikipedia.org/wiki/R-tree
module Data.SpaceTree.R.Explicit where

import Data.Classified
import Data.SpaceTree.Coords
import Data.SpaceTree.Explicit


-- | An object with spatial extent, indexed via a reference point.
--
-- Ideally that point should be in some sense central to the object.
class Coordinate b p i => Blob a b p i | a -> b, a -> p, a -> i where
  reference :: a -> p
  bounds :: a -> b

data BlobMeasure a b p i = BlobMeasure {
  blobCount :: Int,
  blobBounds :: Maybe b
} deriving (Eq, Show)

instance Blob a b p i => Semigroup (BlobMeasure a b p i) where
  BlobMeasure m a <> BlobMeasure n b = BlobMeasure (m + n) (a <> b)

instance Blob a b p i => Monoid (BlobMeasure a b p i) where
  mempty = BlobMeasure 0 mempty

instance Blob a b p i => Measure (BlobMeasure a b p i) a where
  measure a = BlobMeasure 1 (Just (bounds a))


newtype BlobTree p i b m a v = BlobTree {
  blobs :: SpaceTree (BlobMeasure a b p i) p i b m (a, v)
}

blobMeasure :: Blob a b p i => BlobTree p i b m a v -> BlobMeasure a b p i
blobMeasure (BlobTree t) = case t of
  Empty              -> mempty
  Singleton _ (a, _) -> measure a
  Branch n _         -> n

instance Blob a b p i => Measure (BlobMeasure a b p i) (BlobTree p i b m a v) where
  measure = blobMeasure

classifyB :: BlobTree p i b m a v -> Classified a v
classifyB = let
  f Zero           = Zero
  f (One _ (a, v)) = One a v
  f Many           = Many
  in f . classifyT . blobs

nonNullB :: BlobTree p i b m a v -> Maybe (BlobTree p i b m a v)
nonNullB (BlobTree t) = BlobTree <$> nonNullT t

sizeB :: (Blob a b p i) => BlobTree p i b m a v -> Int
sizeB = blobCount . blobMeasure

