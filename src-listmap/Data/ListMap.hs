{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-- | Maps backed by lists of key/value pairs, in increasing order of
-- key. This is unlikely to be a very attractive data structure, but
-- it's nice to demonstrate that the same technology works.
module Data.ListMap where

import Control.Applicative (liftA2)

import Data.Filterable.WithIndex
import Data.Foldable.WithIndex
import Data.Functor.WithIndex
import Data.Traversable.WithIndex
import Data.Witherable.WithIndex

import Data.MergeTactics        (WhenMissing(..),
                                 runWhenMissing,
                                 missingKey,
                                 WhenMatched(..))


newtype ListMap k v = ListMap {
  kvPairs :: [(k, v)]
} deriving (Eq, Ord)

empty :: ListMap k v
empty = ListMap []

singleton :: k -> v -> ListMap k v
singleton x y = ListMap [(x,y)]

instance Functor (ListMap k) where
  fmap f (ListMap l) = ListMap (fmap f <$> l)

instance FunctorWithIndex k (ListMap k) where
  imap f (ListMap l) = ListMap (imap f <$> l)

instance Foldable (ListMap k) where
  foldMap f (ListMap l) = foldMap (f . snd) l
  foldr f x (ListMap l) = foldr (f . snd) x l

instance FoldableWithIndex k (ListMap k) where
  ifoldMap f (ListMap l) = foldMap (uncurry f) l
  ifoldr f x (ListMap l) = foldr (uncurry f) x l

instance Traversable (ListMap k) where
  traverse f (ListMap l) = ListMap <$> traverse (traverse f) l

instance TraversableWithIndex k (ListMap k) where
  itraverse f (ListMap l) = ListMap <$> traverse (itraverse f) l

instance Filterable (ListMap k) where
  mapMaybe f (ListMap l) = ListMap $ mapMaybe (traverse f) l

instance FilterableWithIndex k (ListMap k) where
  imapMaybe f (ListMap l) = ListMap $ mapMaybe (itraverse f) l

instance Witherable (ListMap k) where
  wither f (ListMap l) = ListMap <$> wither (fmap sequenceA . traverse f) l

instance WitherableWithIndex k (ListMap k) where
  iwither f (ListMap l) = ListMap <$> wither (fmap sequenceA . itraverse f) l

alterF :: (Functor f, Ord k) => (Maybe a -> f (Maybe a)) -> k -> ListMap k a -> f (ListMap k a)
alterF f x (ListMap a) = let

  g l Nothing = l
  g l (Just u) = ((x,u):l)
  
  go [] = g [] <$> f Nothing
  go l@((y,v):l') = case compare x y of
    LT -> g l <$> f Nothing
    EQ -> g l' <$> f (Just v)
    GT -> ((y,v):) <$> go l'
  
  in ListMap <$> go a

mergeA  :: (Applicative f, Ord k)
  => WhenMissing f k a c -- ^ What to do with keys in @m1@ but not @m2@
  -> WhenMissing f k b c -- ^ What to do with keys in @m2@ but not @m1@
  -> WhenMatched f k a b c -- ^ What to do with keys in both @m1@ and @m2@
  -> ListMap k a -- ^ Map @m1@
  -> ListMap k b -- ^ Map @m2@
  -> f (ListMap k c)
mergeA onL onR onB (ListMap a1) (ListMap a2) = let

  maybeCons _ Nothing l = l
  maybeCons x (Just y) l = (x,y):l
  
  go l1 [] = kvPairs <$> runWhenMissing onL (ListMap l1)
  go [] l2 = kvPairs <$> runWhenMissing onR (ListMap l2)
  go l1@((x,u):l1') l2@((y,v):l2') = case compare x y of
    LT -> liftA2 (maybeCons x) (missingKey onL x u) (go l1' l2)
    EQ -> liftA2 (maybeCons x) (matchedKey onB x u v) (go l1' l2')
    GT -> liftA2 (maybeCons y) (missingKey onR y v) (go l1 l2')

  in ListMap <$> go a1 a2
