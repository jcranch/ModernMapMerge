{-# LANGUAGE
      EmptyCase,
      FlexibleInstances,
      FunctionalDependencies,
      Safe
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

-- | Support for data structures whose elements can be removed and
-- traversed.
module Data.Witherable (
  Functor(..),
  Foldable(..),
  Traversable(..),
  Filterable(..),
  Witherable(..),
  -- helper functions from Data.Filterable
  (<$?>),
  (<&?>),
  -- helper functions from this module
  mapMaybeByWithering,
  forMaybe,
  ) where

import Control.Applicative
import Data.Bool (bool)
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Functor.Product as Product
import Data.Functor.Sum as Sum
import Data.Monoid
import Data.Proxy
import Data.Traversable
import Prelude hiding (filter)
import qualified Data.Foldable as F
import qualified Data.IntMap.Lazy as IM
import qualified Data.Map.Lazy as M
import qualified Data.Sequence as S
import qualified GHC.Generics as Generics

import Data.Filterable


-- | An enhancement of 'Traversable' with 'Filterable'
--
-- A definition of 'wither' must satisfy the following laws:
--
-- [/conservation/]
--   @'wither' ('fmap' 'Just' . f) ≡ 'traverse' f@
--
-- [/composition/]
--   @'Compose' . 'fmap' ('wither' f) . 'wither' g ≡ 'wither' ('Compose' . 'fmap' ('wither' f) . g)@
--
-- Parametricity implies the naturality law:
--
-- Whenever @t@ is an //applicative transformation// in the sense described in the
-- 'Traversable' documentation,
--
--   @t . 'wither' f ≡ 'wither' (t . f)@

class (Traversable t,
       Filterable t)
    => Witherable t where

  -- | Effectful 'mapMaybe'.
  --
  -- @'wither' ('pure' . f) ≡ 'pure' . 'mapMaybe' f@
  wither :: Applicative f => (a -> f (Maybe b)) -> t a -> f (t b)
  wither f = fmap catMaybes . traverse f
  {-# INLINE wither #-}

  -- | Monadic variant of 'wither'. This may have more efficient implementation.
  witherM :: Monad m => (a -> m (Maybe b)) -> t a -> m (t b)
  witherM = wither

  filterA :: Applicative f => (a -> f Bool) -> t a -> f (t a)
  filterA f = wither $ \a -> (\b -> if b then Just a else Nothing) <$> f a

  -- | Monadic variant of 'filter', implemented by default in terms of
  -- 'witherM'.
  filterM :: Monad m => (a -> m Bool) -> t a -> m (t a)
  filterM f = witherM $ \a -> (\b -> if b then Just a else Nothing) <$> f a

  witherMap :: (Applicative m) => (t b -> r) -> (a -> m (Maybe b)) -> t a -> m r
  witherMap p f = fmap p . wither f
  {-# INLINE witherMap #-}

  {-# MINIMAL #-}

-- | A default `mapMaybe` obtained by running `wither`.
mapMaybeByWithering :: (Witherable t) => (a -> Maybe b) -> t a -> t b
mapMaybeByWithering f = runIdentity . wither (Identity . f)

instance Witherable Maybe where
  wither _ Nothing = pure Nothing
  wither f (Just a) = f a
  {-# INLINABLE wither #-}

instance Monoid e => Witherable (Either e) where
  wither _ (Left e) = pure (Left e)
  wither f (Right a) = fmap (maybe (Left mempty) Right) (f a)
  {-# INLINABLE wither #-}

-- | Methods are good consumers for fusion.
instance Witherable [] where
  wither f = foldr go (pure []) where
    go x = liftA2 (maybe id (:)) (f x)
  {-# INLINE wither #-}
  witherM f = foldr go (pure []) where
    go x r = f x >>=
      (\z -> case z of
        Nothing -> r
        Just y -> (y:) <$> r
      )
  {-# INLINE witherM #-}

  -- Compared to the default, this fuses an fmap into a liftA2.
  filterA p = go where
    go (x:xs) = liftA2 (bool id (x :)) (p x) (go xs)
    go [] = pure []

instance Witherable ZipList where
  wither f = fmap ZipList . wither f . getZipList

instance Witherable IM.IntMap where

instance Witherable (M.Map k) where
  wither f = M.traverseMaybeWithKey (const f)

instance Witherable Proxy where
  wither _ Proxy = pure Proxy

instance Witherable (Const r) where
  wither _ (Const r) = pure (Const r)
  {-# INLINABLE wither #-}

instance Witherable S.Seq where
  wither f = fmap S.fromList . wither f . F.toList
  {-# INLINABLE wither #-}

{-
  -- TODO: try to figure out whether the following is better or worse for
  -- typical applications. It builds the sequence incrementally rather than
  -- building a list and converting.  This is basically the same approach
  -- currently used by Data.Sequence.filter.

  witherM f = F.foldlM go S.empty
    where
      --go :: S.Seq b -> a -> m (S.Seq b)
      go s a = do
        mb <- f a
        case mb of
          Nothing -> pure s
          Just b -> pure $! s S.|> b
  {-# INLINABLE witherM #-}
-}

instance (Traversable f, Witherable g) => Witherable (Compose f g) where
  wither f = fmap Compose . traverse (wither f) . getCompose
  witherM f = fmap Compose . mapM (witherM f) . getCompose
  filterA p = fmap Compose . traverse (filterA p) . getCompose

instance (Witherable f, Witherable g) => Witherable (Product.Product f g) where
  wither f (Pair x y) = liftA2 Pair (wither f x) (wither f y)
  witherM f (Pair x y) = liftA2 Pair (witherM f x) (witherM f y)
  filterA p (Pair x y) = liftA2 Pair (filterA p x) (filterA p y)

instance (Witherable f, Witherable g) => Witherable (Sum.Sum f g) where
  wither f (InL x) = InL <$> wither f x
  wither f (InR y) = InR <$> wither f y

  witherM f (InL x) = InL <$> witherM f x
  witherM f (InR y) = InR <$> witherM f y

  filterA f (InL x) = InL <$> filterA f x
  filterA f (InR y) = InR <$> filterA f y

instance Witherable Generics.V1 where
  wither _ v = pure $ case v of {}
  filterA _ v = pure $ case v of {}

instance Witherable Generics.U1 where
  wither _ _ = pure Generics.U1
  filterA _ _ = pure Generics.U1

instance Witherable (Generics.K1 i c) where
  wither _ (Generics.K1 a) = pure (Generics.K1 a)
  filterA _ (Generics.K1 a) = pure (Generics.K1 a)

instance Witherable f => Witherable (Generics.Rec1 f) where
  wither f (Generics.Rec1 a) = fmap Generics.Rec1 (wither f a)
  witherM f (Generics.Rec1 a) = fmap Generics.Rec1 (witherM f a)
  filterA f (Generics.Rec1 a) = fmap Generics.Rec1 (filterA f a)

instance Witherable f => Witherable (Generics.M1 i c f) where
  wither f (Generics.M1 a) = fmap Generics.M1 (wither f a)
  witherM f (Generics.M1 a) = fmap Generics.M1 (witherM f a)
  filterA f (Generics.M1 a) = fmap Generics.M1 (filterA f a)

instance (Witherable f, Witherable g) => Witherable ((Generics.:*:) f g) where
  wither f (a Generics.:*: b) = liftA2 (Generics.:*:) (wither f a) (wither f b)
  witherM f (a Generics.:*: b) = liftA2 (Generics.:*:) (witherM f a) (witherM f b)
  filterA f (a Generics.:*: b) = liftA2 (Generics.:*:) (filterA f a) (filterA f b)

instance (Witherable f, Witherable g) => Witherable ((Generics.:+:) f g) where
  wither f (Generics.L1 a) = fmap Generics.L1 (wither f a)
  wither f (Generics.R1 a) = fmap Generics.R1 (wither f a)
  witherM f (Generics.L1 a) = fmap Generics.L1 (witherM f a)
  witherM f (Generics.R1 a) = fmap Generics.R1 (witherM f a)
  filterA f (Generics.L1 a) = fmap Generics.L1 (filterA f a)
  filterA f (Generics.R1 a) = fmap Generics.R1 (filterA f a)

instance (Traversable f, Witherable g) => Witherable ((Generics.:.:) f g) where
  wither f = fmap Generics.Comp1 . traverse (wither f) . Generics.unComp1
  witherM f = fmap Generics.Comp1 . mapM (witherM f) . Generics.unComp1
  filterA f = fmap Generics.Comp1 . traverse (filterA f) . Generics.unComp1

-- | @'forMaybe' = 'flip' 'wither'@
forMaybe :: (Witherable t, Applicative f) => t a -> (a -> f (Maybe b)) -> f (t b)
forMaybe = flip wither
{-# INLINE forMaybe #-}
