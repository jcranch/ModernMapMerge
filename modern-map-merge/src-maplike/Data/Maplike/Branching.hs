{-# LANGUAGE
      DeriveFunctor,
      FlexibleContexts,
      MultiParamTypeClasses,
      UndecidableInstances
  #-}


-- TODO do nonNull on the outer as well as the inner maps


-- | A variant of prefix maps: given a `Maplike` with keys `Maybe`
-- @k@, we provide a Maplike with keys `Map` @r k@.
module Data.Maplike.Branching where

import Prelude hiding (null)

import Data.Foldable.WithIndex
import Data.Functor.WithIndex
import Data.Map.Strict (Map)
import Data.Traversable.WithIndex
import Witherable
import Data.MergeTactics (WhenMissing(..),
                          WhenMatched(..),
                          reindexMissing,
                          traverseMaybeMissing)

import qualified Data.Map.Strict as M

import Data.Maplike


-- | Insert or not depending on Maybe
maybeInsert :: Maplike k m => k -> Maybe v -> m v -> m v
maybeInsert _ Nothing  = id
maybeInsert k (Just v) = insert k v


-- TODO Probably want strict Maybes and a strict pair
data Branching r m v = Branching {
  content :: Maybe v,
  children :: Maybe (r, m (Branching r m v))
} deriving (Functor)

instance (FunctorWithIndex (Maybe k) m, Ord r) => FunctorWithIndex (Map r k) (Branching r m) where
  imap f = let
    go m (Branching h t) = Branching (f m <$> h) (imap (\r -> imap (\k -> go (maybeInsert r k m))) <$> t)
    in go M.empty

instance Foldable m => Foldable (Branching r m) where
  foldr f x (Branching h t) = let
    x' = case t of
      Nothing     -> x
      Just (_, m) -> foldr (flip (foldr f)) x m
    x'' = case h of
      Nothing -> x'
      Just a  -> f a x'
    in x''

instance (FoldableWithIndex (Maybe k) m, Ord r) => FoldableWithIndex (Map r k) (Branching r m) where

  ifoldMap f = let
    go u (Branching h t) = let
      onT (r, m) = ifoldMap (\z -> go (maybeInsert r z u)) m
      in foldMap (f u) h <> foldMap onT t
    in go M.empty
  
  ifoldr f = let
    go u x (Branching h t) = let
      x' = case t of
        Nothing     -> x
        Just (r, m) -> ifoldr (\z -> flip (go (maybeInsert r z u))) x m
      x'' = case h of
        Nothing -> x'
        Just a  -> f u a x'
      in x''
    in go M.empty

instance Traversable m => Traversable (Branching r m) where
  traverse f = let
    g (Branching h t) = liftA2 Branching (traverse f h) (traverse (traverse (traverse g)) t)
    in g

instance (TraversableWithIndex (Maybe k) m, Ord r) => TraversableWithIndex (Map r k) (Branching r m) where
  itraverse f = let
    go u (Branching h t) = liftA2 Branching (traverse (f u) h) (traverse (itraverse (\r -> itraverse (\z -> go (maybeInsert r z u)))) t)
    in go M.empty

instance Maplike (Maybe k) m => Filterable (Branching r m) where
  mapMaybe f = let
    inner (Branching h t) = Branching (f =<< h) (mapMaybe
