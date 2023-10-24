{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-- | This provides support for reindexing a datatype along a function
module Data.MergeTactics.Reindex where

import Data.Functor.WithIndex
import Data.Foldable.WithIndex
import Data.Traversable.WithIndex

import Witherable

import Data.MergeTactics


-- | If @n x@ has indices of type @i@, then @Reindexed i j n x@ has
-- indices of type @j@.
data Reindexed i j n x = Reindexed {
  reindexing :: i -> j,
  underlying :: n x
}

instance (Functor n) => Functor (Reindexed i j n) where
  fmap f (Reindexed r m) = Reindexed r $ fmap f m

instance (FunctorWithIndex i n) => FunctorWithIndex j (Reindexed i j n) where
  imap f (Reindexed r m) = Reindexed r $ imap (f . r) m

instance Foldable n => Foldable (Reindexed i j n) where
  foldMap f (Reindexed _ m) = foldMap f m

instance (FoldableWithIndex i n) => FoldableWithIndex j (Reindexed i j n) where
  ifoldMap f (Reindexed r m) = ifoldMap (f . r) m

instance Traversable n => Traversable (Reindexed i j n) where
  traverse f (Reindexed r m) = Reindexed r <$> traverse f m

instance TraversableWithIndex i n => TraversableWithIndex j (Reindexed i j n) where
  itraverse f (Reindexed r m) = Reindexed r <$> itraverse (f . r) m

instance Filterable n => Filterable (Reindexed i j n) where
  mapMaybe f (Reindexed r m) = Reindexed r $ mapMaybe f m

instance FilterableWithIndex i n => FilterableWithIndex j (Reindexed i j n) where
  imapMaybe f (Reindexed r m) = Reindexed r $ imapMaybe (f . r) m

instance Witherable n => Witherable (Reindexed i j n) where
  wither f (Reindexed r m) = Reindexed r <$> wither f m

instance WitherableWithIndex i n => WitherableWithIndex j (Reindexed i j n) where
  iwither f (Reindexed r m) = Reindexed r <$> iwither (f . r) m


-- | This avoids nesting instances of `Reindexed`.
rereindex :: (j -> k) -> Reindexed i j n x -> Reindexed i k n x
rereindex f (Reindexed r m) = Reindexed (f . r) m


-- | We can use this formalism to change the indices in a
-- `WhenMissing`. This construction is why the code was written.
reindexWithering :: (Functor f) => (i -> j) -> WhenMissing f j x y -> WhenMissing f i x y
reindexWithering r w = WhenMissing (fmap underlying . runWhenMissing w . Reindexed r)
