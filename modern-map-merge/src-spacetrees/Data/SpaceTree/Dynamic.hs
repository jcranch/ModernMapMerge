{-# LANGUAGE
      DeriveFunctor,
      FlexibleInstances,
      MultiParamTypeClasses
  #-}

module Data.SpaceTree.Dynamic where

import Data.Foldable.WithIndex
import Data.Functor.WithIndex
import Data.Traversable.WithIndex
import Witherable

import Data.Maplike
import Data.SpaceTree.Coords
import Data.SpaceTree.Explicit


data DynamicMap p i b m v = DynamicMap {
  getBounds :: Maybe b,
  getTree :: SpaceTree p i b m v
} deriving (Functor)

instance Functor m => FunctorWithIndex p (DynamicMap p i b m) where
  imap f (DynamicMap b s) = DynamicMap b $ imap f s

instance Foldable m => Foldable (DynamicMap p i b m) where
  foldr f z (DynamicMap _ m) = foldr f z m
  foldMap f (DynamicMap _ m) = foldMap f m

instance Foldable m => FoldableWithIndex p (DynamicMap p i b m) where
  ifoldr f z (DynamicMap _ m) = ifoldr f z m
  ifoldMap f (DynamicMap _ m) = ifoldMap f m

instance Traversable m => Traversable (DynamicMap p i b m) where
  traverse f (DynamicMap b m) = DynamicMap b <$> traverse f m

instance Traversable m => TraversableWithIndex p (DynamicMap p i b m) where
  itraverse f (DynamicMap b m) = DynamicMap b <$> itraverse f m

instance Maplike i m => Filterable (DynamicMap p i b m) where
  mapMaybe f (DynamicMap b m) = DynamicMap b $ mapMaybe f m

instance Maplike i m => FilterableWithIndex p (DynamicMap p i b m) where
  imapMaybe f (DynamicMap b m) = DynamicMap b $ imapMaybe f m

instance Maplike i m => Witherable (DynamicMap p i b m) where
  wither f (DynamicMap b m) = DynamicMap b <$> wither f m

instance Maplike i m => WitherableWithIndex p (DynamicMap p i b m) where
  iwither f (DynamicMap b m) = DynamicMap b <$> iwither f m

instance (Coordinate b p i, Maplike i m) => Maplike p (DynamicMap p i b m) where

  empty = DynamicMap Nothing Empty

  null (DynamicMap _ m) = nullT m

  singleton p v = DynamicMap (Just (pointBox p)) (Singleton p v)

