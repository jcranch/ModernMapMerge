{-# LANGUAGE
      FlexibleInstances,
      MultiParamTypeClasses
  #-}

-- | We define a data type for `Data.Set`, with a dummy variable,
-- whose only purpose is to give us access to the hierarchy of
-- typeclasses up to `WitherableWithIndex`.
--
-- Provided you don't try to use the dummy indices, all is well.
module Data.Set.Merge.Internal where

import Control.Applicative (liftA3)
import Data.Foldable.WithIndex
import Data.Functor.WithIndex
import Data.Traversable.WithIndex
import Data.Monoid (Ap(..))
import Data.Semigroup (stimesMonoid)
import Data.Set.Internal (Set(..))
import qualified Data.Set.Internal as S
import Witherable


newtype Set' k v = Set' {
  getSet :: Set k
}

instance Functor (Set' k) where
  fmap _ (Set' s) = Set' s

instance FunctorWithIndex k (Set' k) where
  imap _ (Set' s) = Set' s

instance Foldable (Set' k) where
  foldMap f (Set' s) = S.size s `stimesMonoid` f undefined

instance FoldableWithIndex k (Set' k) where
  ifoldr f a (Set' s) = let
    f' x = f x undefined
    in S.foldr f' a s

instance Traversable (Set' k) where
  traverse f (Set' s) = let
    Ap m = S.size s `stimesMonoid` (Ap (const () <$> f undefined))
    in Set' <$> (m *> pure s)

instance TraversableWithIndex k (Set' k) where
  itraverse f (Set' s) = let
    go Tip = pure Tip
    go (Bin n x l r) = liftA2 (Bin n x) (f x undefined *> go l) (go r)
    in Set' <$> go s

instance Filterable (Set' k) where
  mapMaybe f (Set' s) = Set' $ case f undefined of
    Just _  -> s
    Nothing -> S.empty

instance FilterableWithIndex k (Set' k) where
  imapMaybe f (Set' s) = let
    f' x = case f x undefined of
      Just _  -> True
      Nothing -> False
    in Set' (S.filter f' s)

instance Witherable (Set' k)

-- In the innards of the WitherableWithIndex implementation below, one
-- would like to write the following. This would avoid creating
-- duplicate Bin nodes, but we don't have access to the internals of
-- the containers package:
{-
    go b@(Bin _ x l r) = let
      assemble (Just _) l' r'
        | l `ptrEq` l' && r `ptrEq` r' = b
        | otherwise                    = S.link x l' r'
      assemble Nothing l' r'           = S.merge l' r'
-}
instance WitherableWithIndex k (Set' k) where
  iwither f (Set' s) = let
    go Tip = pure Tip
    go (Bin _ x l r) = let
      assemble (Just _)          = S.link x
      assemble Nothing           = S.merge
      in liftA3 assemble (f x undefined) (go l) (go r)
    in Set' <$> go s
