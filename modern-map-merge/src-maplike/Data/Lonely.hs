{-# LANGUAGE
      DeriveFunctor,
      FlexibleInstances,
      MultiParamTypeClasses
  #-}

-- | A degenerate `Maplike` which can have at most one key; an
-- exception is raised if there is any attempt to add a second.
--
-- This is, of course, not normally of much use, but can be useful in
-- compositional `Maplike` instances.
module Data.Lonely where

import Data.Foldable.WithIndex (FoldableWithIndex(..))
import Data.Functor.WithIndex (FunctorWithIndex(..))
import Data.Traversable.WithIndex (TraversableWithIndex(..))

import Data.MergeTactics
import Witherable (Filterable(..),
                   Witherable(..),
                   FilterableWithIndex(..),
                   WitherableWithIndex(..))

import Data.Maplike


data Lonely k v =
  Barren |
  Alone k v
  deriving (Eq, Functor, Ord, Show)

maybeSingleton' :: k -> Maybe v -> Lonely k v
maybeSingleton' _ Nothing  = Barren
maybeSingleton' k (Just v) = Alone k v 

instance Foldable (Lonely k) where
  foldMap _ Barren      = mempty
  foldMap f (Alone _ v) = f v

instance Traversable (Lonely k) where
  traverse _ Barren      = pure Barren
  traverse f (Alone k v) = Alone k <$> f v

instance Filterable (Lonely k) where
  mapMaybe _ Barren      = Barren
  mapMaybe f (Alone k v) = case f v of
    Nothing -> Barren
    Just w  -> Alone k w

instance Witherable (Lonely k) where
  wither _ Barren      = pure Barren
  wither f (Alone k v) = let
    g Nothing  = Barren
    g (Just w) = Alone k w
    in g <$> f v

instance FunctorWithIndex k (Lonely k) where
  imap _ Barren      = Barren
  imap f (Alone k v) = Alone k (f k v)

instance FoldableWithIndex k (Lonely k) where
  ifoldMap _ Barren      = mempty
  ifoldMap f (Alone k v) = f k v

instance TraversableWithIndex k (Lonely k) where
  itraverse _ Barren      = pure Barren
  itraverse f (Alone k v) = Alone k <$> f k v

instance FilterableWithIndex k (Lonely k) where
  imapMaybe _ Barren      = Barren
  imapMaybe f (Alone k v) = maybeSingleton' k (f k v)

instance WitherableWithIndex k (Lonely k) where
  iwither _ Barren      = pure Barren
  iwither f (Alone k v) = maybeSingleton' k <$> f k v

instance Eq k => Maplike k (Lonely k) where

  empty = Barren

  null Barren      = True
  null (Alone _ _) = False

  singleton = Alone

  alterF f k Barren       = maybeSingleton' k <$> f Nothing
  alterF f k (Alone k' v)
    | k == k'             = maybeSingleton' k <$> f (Just v)
    | otherwise           = let
        h Nothing  = Alone k' v
        h (Just _) = error "Lonely.alterF: attempt to add second key"
        in h <$> f Nothing

  alterMinWithKeyF _ Barren      = Nothing
  alterMinWithKeyF f (Alone k v) = Just (maybeSingleton' k <$> f k v)

  alterMaxWithKeyF = alterMinWithKeyF

  imergeA onL onR onB = let
    m Barren      Barren      = pure Barren
    m Barren      (Alone l v) = maybeSingleton' l <$> missingKey onR l v
    m (Alone k u) Barren      = maybeSingleton' k <$> missingKey onL k u
    m (Alone k u) (Alone l v)
      | k == l    = maybeSingleton' k <$> matchedKey onB k u v
      | otherwise = let
          g Nothing  Nothing  = Barren
          g (Just w) Nothing  = Alone k w
          g Nothing  (Just w) = Alone l w
          g (Just _) (Just _) = error "Lonely.imergeA: attempt to merge two keys"
          in liftA2 g (missingKey onL k u) (missingKey onR l v)
    in m
