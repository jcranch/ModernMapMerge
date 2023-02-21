{-# LANGUAGE
      FlexibleInstances,
      FunctionalDependencies,
      UndecidableInstances
  #-}

module Data.Functor.ExtraUndecidable where

import Control.Applicative (liftA2)
import Data.Functor.Compose
import Data.Functor.Product as P
import Data.Functor.Sum as Sum

import Data.Functor.WithIndex
import Data.Traversable.WithIndex
import Data.Filterable.WithIndex
import Data.Witherable.WithIndex

instance (FunctorWithIndex i f, FilterableWithIndex j g) => FilterableWithIndex (i, j) (Compose f g) where
  imapMaybe f = Compose . imap (\i -> imapMaybe (\j -> f (i, j))) . getCompose
  ifilter p = Compose . imap (\i -> ifilter (\j -> p (i, j))) . getCompose

instance (FilterableWithIndex i f, FilterableWithIndex j g) => FilterableWithIndex (Either i j) (P.Product f g) where
  imapMaybe f (P.Pair x y) = P.Pair (imapMaybe (f . Left) x) (imapMaybe (f . Right) y)
  ifilter p (P.Pair x y) = P.Pair (ifilter (p . Left) x) (ifilter (p . Right) y)

instance (FilterableWithIndex i f, FilterableWithIndex j g) => FilterableWithIndex (Either i j) (Sum.Sum f g) where
  imapMaybe f (Sum.InL x) = Sum.InL (imapMaybe (f . Left) x)
  imapMaybe f (Sum.InR y) = Sum.InR (imapMaybe (f . Right) y)

  ifilter f (Sum.InL x) = Sum.InL (ifilter (f . Left) x)
  ifilter f (Sum.InR y) = Sum.InR (ifilter (f . Right) y)

instance (TraversableWithIndex i f, WitherableWithIndex j g) => WitherableWithIndex (i, j) (Compose f g) where
  iwither f = fmap Compose . itraverse (\i -> iwither (\j -> f (i, j))) . getCompose
  iwitherM f = fmap Compose . imapM (\i -> iwitherM (\j -> f (i, j))) . getCompose
  ifilterA p = fmap Compose . itraverse (\i -> ifilterA (\j -> p (i, j))) . getCompose

instance (WitherableWithIndex i f, WitherableWithIndex j g) => WitherableWithIndex (Either i j) (P.Product f g) where
  iwither f (P.Pair x y) = liftA2 P.Pair (iwither (f . Left) x) (iwither (f . Right) y)
  iwitherM f (P.Pair x y) = liftA2 P.Pair (iwitherM (f . Left) x) (iwitherM (f . Right) y)
  ifilterA p (P.Pair x y) = liftA2 P.Pair (ifilterA (p . Left) x) (ifilterA (p . Right) y)
