{-# LANGUAGE
      DeriveFunctor,
      FlexibleInstances,
      MultiParamTypeClasses
  #-}

-- | Spacetrees together with a bounding box.
--
-- Algorithms do not shrink the box unless asked to do so
-- specifically.
module Data.SpaceTree.Dynamic where

import Data.Functor.Compose (Compose(..))
import Data.Foldable.WithIndex
import Data.Functor.WithIndex
import Data.Traversable.WithIndex
import Witherable

import Data.Maplike
import Data.MergeTactics
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

  alterF f p (DynamicMap Nothing  _) = maybeSingleton p <$> f Nothing
  alterF f p (DynamicMap (Just b) m) = let
    g Nothing  = (False, Nothing)
    g (Just x) = (True,  Just x)
    b' = b <> pointBox p
    h (False, t) = DynamicMap (Just b)  t
    h (True,  t) = DynamicMap (Just b') t
    in fmap h . getCompose $ alterFT (Compose . fmap g . f) p b m

  alterMinWithKeyF _ (DynamicMap Nothing  _) = Nothing
  alterMinWithKeyF f (DynamicMap (Just b) m) = fmap (DynamicMap (Just b)) <$> alterMinWithKeyFT f b m

  alterMaxWithKeyF _ (DynamicMap Nothing  _) = Nothing
  alterMaxWithKeyF f (DynamicMap (Just b) m) = fmap (DynamicMap (Just b)) <$> alterMaxWithKeyFT f b m

  -- For efficiency, this has to be reasonably complicated. We do a
  -- more complicated version of the filtering code, where we extract
  -- a point at a time and find its subbox. However, we want four
  -- versions, depending on whether we can reuse the bounding box of
  -- either argument.
  imergeA onL onR onB = let

    toB b m n = _
    
    toL b c m n = _
    
    toR b c m n = _
    
    toU b c d m n = _

    go (DynamicMap Nothing _)  (DynamicMap Nothing _)  = pure empty
    go (DynamicMap (Just b) m) (DynamicMap Nothing _)  = DynamicMap (Just b) <$> runWhenMissing onL m
    go (DynamicMap Nothing _)  (DynamicMap (Just c) n) = DynamicMap (Just c) <$> runWhenMissing onR n
    go (DynamicMap (Just b) m) (DynamicMap (Just c) n)
      | b == c       = toB b m n
      | isSubset c b = toL b c m n
      | isSubset b c = toR b c m n
      | otherwise    = toU b c (b <> c) m n

    in go

-- | What's the tightest possible bounding box?
extent :: (Coordinate b p i, Maplike i m) => DynamicMap p i b m v -> Maybe b
extent (DynamicMap Nothing  _) = Nothing
extent (DynamicMap (Just b) m) = findExtent (\r s -> findBestT (Just . r) (Just . s) b) m

