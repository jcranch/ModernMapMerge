{-# LANGUAGE
      FlexibleInstances,
      FunctionalDependencies,
      Safe
  #-}

-- This file has an orphan instance (FoldableWithIndex, for Kleisli)
-- which I have attempted to move upstream.
{-# OPTIONS_GHC
      -fno-warn-orphans
  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Witherable
-- Copyright   :  (c) Fumiaki Kinoshita 2014-23, James Cranch 2021-23
-- License     :  BSD3
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
-----------------------------------------------------------------------------

-- | Support for indexed data structures whose elements can be
-- selectively removed.
module Data.Filterable.WithIndex (
  Functor(..),
  FunctorWithIndex(..),
  Filterable(..),
  FilterableWithIndex(..),
  -- helper functions from Data.Filterable
  (<$?>),
  (<&?>),
  -- helper functions from this module
  imapMaybeList,
) where

import Prelude hiding (filter)

import Control.Arrow (Kleisli(..))
import Control.Monad (MonadPlus, mzero)
import Data.Functor.WithIndex
import Data.Proxy
import Data.Void
import qualified Data.IntMap.Lazy as IM
import qualified Data.Map.Lazy as M

import Data.Filterable


-- | Indexed variant of 'Filterable'.
class (FunctorWithIndex i t,
       Filterable t)
    => FilterableWithIndex i t | t -> i where
  imapMaybe :: (i -> a -> Maybe b) -> t a -> t b
  imapMaybe f = catMaybes . imap f
  {-# INLINE imapMaybe #-}

  -- | @'ifilter' f . 'ifilter' g ≡ ifilter (\i -> 'Control.Applicative.liftA2' ('&&') (f i) (g i))@
  ifilter :: (i -> a -> Bool) -> t a -> t a
  ifilter f = imapMaybe $ \i a -> if f i a then Just a else Nothing
  {-# INLINE ifilter #-}

instance FilterableWithIndex () Maybe

instance FilterableWithIndex Int IM.IntMap where
  imapMaybe = IM.mapMaybeWithKey
  ifilter = IM.filterWithKey

instance FilterableWithIndex k (M.Map k) where
  imapMaybe = M.mapMaybeWithKey
  ifilter = M.filterWithKey

instance FilterableWithIndex Void Proxy

-- Should be put in indexed-traversable
-- cf https://github.com/haskellari/indexed-traversable/pull/19
instance Functor m => FunctorWithIndex a (Kleisli m a) where
  imap f (Kleisli p) = Kleisli (\x -> fmap (f x) (p x))


instance MonadPlus m => FilterableWithIndex a (Kleisli m a) where
  imapMaybe f (Kleisli p) = Kleisli (\x -> maybe mzero pure . f x =<< p x)

-- | Lists are not FilterableWithIndex, despite being FunctorWithIndex
-- and Filterable, since filtering changes the indices.
-- 
-- However, we provide the nonconformant functionality with a
-- different name.
imapMaybeList :: (Int -> a -> Maybe b) -> [a] -> [b]
imapMaybeList f = mapMaybe (uncurry f) . zip [0..]
