{-# LANGUAGE
      FlexibleInstances,
      GeneralizedNewtypeDeriving,
      MultiParamTypeClasses,
      QuantifiedConstraints,
      StandaloneDeriving
  #-}

-- | Spacetrees where there is a canonical bounding box
module Data.SpaceTree.Default where

import Data.Foldable.WithIndex
import Data.Functor.WithIndex
import Data.Monoid (Sum(..))
import Data.Traversable.WithIndex
import Witherable

import Data.Maplike
import Data.SpaceTree.Coords
import Data.SpaceTree.Explicit


newtype DefaultMap p i b m v = DefaultMap {
  getSpaceTree :: SpaceTree (Sum Int) p i b m v
} deriving (Functor,
            FunctorWithIndex p,
            Foldable,
            FoldableWithIndex p,
            Filterable,
            FilterableWithIndex p)

deriving instance (Eq p, forall x. Eq x => Eq (m x), Eq v) => Eq (DefaultMap p i b m v)

instance Traversable m => Traversable (DefaultMap p i b m) where
  traverse f (DefaultMap m) = DefaultMap <$> traverse f m

instance Traversable m => TraversableWithIndex p (DefaultMap p i b m) where
  itraverse f (DefaultMap m) = DefaultMap <$> itraverse f m

instance Maplike i m => Witherable (DefaultMap p i b m) where
  wither f (DefaultMap m) = DefaultMap <$> wither f m

instance Maplike i m => WitherableWithIndex p (DefaultMap p i b m) where
  iwither f (DefaultMap m) = DefaultMap <$> iwither f m

instance (Ord p, DefaultCoordinate b p i, Maplike i m) => Maplike p (DefaultMap p i b m) where

  empty = DefaultMap Empty

  null (DefaultMap m) = nullT m

  singleton p v = DefaultMap $ Singleton p v

  alterMinWithKeyF f (DefaultMap m) = fmap DefaultMap <$> alterMinWithKeyFT f outer m

  alterMaxWithKeyF f (DefaultMap m) = fmap DefaultMap <$> alterMaxWithKeyFT f outer m

  alterF f p (DefaultMap m) = DefaultMap <$> alterFT f p outer m

  imerge l r t (DefaultMap m) (DefaultMap n) = DefaultMap $ imergeT l r t outer m n

  imergeA l r t (DefaultMap m) (DefaultMap n) = DefaultMap <$> imergeAT l r t outer m n
