{-# LANGUAGE
      DeriveTraversable,
      FlexibleInstances,
      FunctionalDependencies,
      GeneralizedNewtypeDeriving,
      Trustworthy
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Witherable
-- Copyright   :  (c) Fumiaki Kinoshita 2020, James Cranch 2021
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
-----------------------------------------------------------------------------

-- | Support for indexed data structures whose elements can be removed
-- and traversed.
module Data.Witherable.WithIndex (
  Witherable(..),
  WitherableWithIndex(..),
  defaultIwither,
  mapMaybeDefault,
  imapMaybeDefault,
  mapMaybeByWithering,
  imapMaybeByWithering,
  iwitherList,
) where

import Control.Applicative
import Data.Foldable.WithIndex
import Data.Functor.Identity
import Data.Functor.WithIndex
import Data.Proxy
import Data.Traversable.WithIndex
import Data.Void
import Prelude hiding (filter)
import qualified Data.Foldable as F
import qualified Data.IntMap.Lazy as IM
import qualified Data.Map.Lazy as M
import qualified Data.Traversable as T

import Data.Filterable
import Data.Filterable.WithIndex
import Data.Witherable


-- | A default withering function; provided separately since it's
-- useful for some data types which are not WitherableWithIndex (such
-- as lists).
defaultIwither :: (Filterable t, TraversableWithIndex i t, Applicative f)
              => (i -> a -> f (Maybe b)) -> t a -> f (t b)
defaultIwither f = fmap catMaybes . itraverse f


-- | Indexed variant of 'Witherable'.
class (FilterableWithIndex i t, TraversableWithIndex i t, Witherable t)
    => WitherableWithIndex i t | t -> i where
  -- | Effectful 'imapMaybe'.
  --
  -- @'iwither' (\ i -> 'pure' . f i) ≡ 'pure' . 'imapMaybe' f@
  iwither :: (Applicative f) => (i -> a -> f (Maybe b)) -> t a -> f (t b)
  iwither = defaultIwither

  -- | Monadic variant of 'iwither'. This may have more efficient
  -- implementation.
  iwitherM :: (Monad m) => (i -> a -> m (Maybe b)) -> t a -> m (t b)
  iwitherM = iwither

  ifilterA :: (Applicative f) => (i -> a -> f Bool) -> t a -> f (t a)
  ifilterA f = iwither (\i a -> (\b -> if b then Just a else Nothing) <$> f i a)

  -- | Monadic variant of 'ifilter'.
  ifilterM :: (Monad m) => (i -> a -> m Bool) -> t a -> m (t a)
  ifilterM f = iwitherM (\i a -> (\b -> if b then Just a else Nothing) <$> f i a)


-- | A default `imapMaybe` obtained by running `iwither`.
imapMaybeByWithering :: (WitherableWithIndex i t) => (i -> a -> Maybe b) -> t a -> t b
imapMaybeByWithering f = runIdentity . iwither (\ i x -> Identity $ f i x)

instance WitherableWithIndex () Maybe

instance WitherableWithIndex Int IM.IntMap where

instance WitherableWithIndex k (M.Map k) where
  iwither = M.traverseMaybeWithKey

instance WitherableWithIndex Void Proxy

-- | A default implementation for 'mapMaybe'.
mapMaybeDefault :: (F.Foldable f, Alternative f) => (a -> Maybe b) -> f a -> f b
mapMaybeDefault p = F.foldr (\x xs -> case p x of
    Just a -> pure a <|> xs
    _ -> xs) empty
{-# INLINABLE mapMaybeDefault #-}

-- | A default implementation for 'imapMaybe'.
imapMaybeDefault :: (FoldableWithIndex i f, Alternative f) => (i -> a -> Maybe b) -> f a -> f b
imapMaybeDefault p = ifoldr (\i x xs -> case p i x of
    Just a -> pure a <|> xs
    _ -> xs) empty
{-# INLINABLE imapMaybeDefault #-}

newtype WrappedFoldable i f a = WrapFilterable {unwrapFoldable :: f a}
  deriving (Functor, F.Foldable, T.Traversable, Applicative, Alternative)

instance (FunctorWithIndex i f) => FunctorWithIndex i (WrappedFoldable i f) where
  imap f = WrapFilterable . imap f . unwrapFoldable

instance (FoldableWithIndex i f) => FoldableWithIndex i (WrappedFoldable i f) where
  ifoldMap f = ifoldMap f . unwrapFoldable

instance (TraversableWithIndex i f) => TraversableWithIndex i (WrappedFoldable i f) where
  itraverse f = fmap WrapFilterable . itraverse f . unwrapFoldable

instance (F.Foldable f, Alternative f) => Filterable (WrappedFoldable i f) where
    {-#INLINE mapMaybe#-}
    mapMaybe = mapMaybeDefault

instance (FunctorWithIndex i f, FoldableWithIndex i f, Alternative f) => FilterableWithIndex i (WrappedFoldable i f) where
  {-# INLINE imapMaybe #-}
  imapMaybe = imapMaybeDefault

instance (Alternative f, T.Traversable f) => Witherable (WrappedFoldable i f)


-- | Just like we did for FilterableWithIndex, we provide the
-- nonconformant functionality as a separate function.
iwitherList :: (Applicative f) => (Int -> a -> f (Maybe b)) -> [a] -> f [b]
iwitherList p = wither (uncurry p) . zip [0..]
