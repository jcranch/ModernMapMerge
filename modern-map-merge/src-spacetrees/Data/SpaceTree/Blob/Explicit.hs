module Data.SpaceTree.Blob.Explicit where

import Data.SpaceTree.Coords
import Data.SpaceTree.Explicit


-- TODO Need to generalise the "explicit" spacetree formalism to have
-- arbitrary monoids, as in the style of the fingertree package. The
-- default/dynamic implementations can be specialised to the counting
-- monoid


newtype BlobTree p i b m a n v = BlobTree {
  blobs :: SpaceTree p i b m (n v)
}

classifyB :: BlobTree p i b m a n v -> Classified a v
classifyB = bindClassify _ _ . blobs

nonNullB :: BlobTree p i b m a n v -> Maybe (BlobTree p i b m a n v)
nonNullB (BlobTree t) = BlobTree <$> nonNullT t

sizeB :: BlobTree
