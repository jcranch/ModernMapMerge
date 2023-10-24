{-# LANGUAGE
      EmptyCase,
      FlexibleInstances,
      FunctionalDependencies,
      Safe
  #-}

-- | Support for data structures whose elements can be selectively
-- removed.
module Data.Filterable where

import Control.Applicative
import Control.Arrow (Kleisli(..))
import Control.Monad (MonadPlus, mzero, (<=<))
import Data.Functor.Compose
import Data.Functor.Product as P
import Data.Functor.Sum as Sum
import Data.Monoid
import Data.Proxy
import Prelude hiding (filter)
import qualified Data.Foldable as F
import qualified Data.IntMap.Lazy as IM
import qualified Data.Map.Lazy as M
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as S
import qualified GHC.Generics as Generics
import qualified Prelude

-- | Like 'Functor', but you can remove elements instead of updating them.
--
-- Formally, the class 'Filterable' represents a functor from @Kleisli Maybe@ to @Hask@.
--
-- A definition of 'mapMaybe' must satisfy the following laws:
--
-- [/conservation/]
--   @'mapMaybe' (Just . f) ≡ 'fmap' f@
--
-- [/composition/]
--   @'mapMaybe' f . 'mapMaybe' g ≡ 'mapMaybe' (f <=< g)@
class (Functor f) => Filterable f where
  -- | Like 'Maybe.mapMaybe'.
  --
  -- Also, @'mapMaybe' f = 'catMaybes' . 'fmap' f@
  mapMaybe :: (a -> Maybe b) -> f a -> f b
  mapMaybe f = catMaybes . fmap f
  {-# INLINE mapMaybe #-}

  -- | @'catMaybes' ≡ 'mapMaybe' 'id'@
  catMaybes :: f (Maybe a) -> f a
  catMaybes = mapMaybe id
  {-# INLINE catMaybes #-}

  -- | @'filter' f . 'filter' g ≡ filter ('liftA2' ('&&') g f)@
  filter :: (a -> Bool) -> f a -> f a
  filter f = mapMaybe $ \a -> if f a then Just a else Nothing
  {-# INLINE filter #-}

  -- | Empty a Filterable.
  --
  -- Normally this is better implemented as a constant map:
  -- @flush _ = someEmptyThing@
  flush :: f a -> f b
  flush = mapMaybe (const Nothing)
  {-# INLINE flush #-}

  {-# MINIMAL mapMaybe | catMaybes #-}

instance Filterable Maybe where
  mapMaybe f = (>>= f)
  {-# INLINE mapMaybe #-}
  flush _ = Nothing

instance Monoid e => Filterable (Either e) where
  mapMaybe _ (Left e) = Left e
  mapMaybe f (Right a) = maybe (Left mempty) Right $ f a
  {-# INLINABLE mapMaybe #-}
  flush _ = Left mempty

instance Filterable [] where
  mapMaybe = Maybe.mapMaybe
  catMaybes = Maybe.catMaybes
  filter = Prelude.filter
  flush _ = []

instance Filterable ZipList where
  mapMaybe f = ZipList . Maybe.mapMaybe f . getZipList
  catMaybes = ZipList . Maybe.catMaybes . getZipList
  filter f = ZipList . Prelude.filter f . getZipList
  flush _ = ZipList []

instance Filterable IM.IntMap where
  mapMaybe = IM.mapMaybe
  filter = IM.filter
  flush _ = IM.empty

instance Filterable (M.Map k) where
  mapMaybe = M.mapMaybe
  filter = M.filter
  flush _ = M.empty

instance Filterable Proxy where
 mapMaybe _ Proxy = Proxy

instance Filterable (Const r) where
  mapMaybe _ (Const r) = Const r
  {-# INLINABLE mapMaybe #-}

instance Filterable S.Seq where
  mapMaybe f = S.fromList . mapMaybe f . F.toList
  {-# INLINABLE mapMaybe #-}
  filter = S.filter


-- The instances for Compose, Product, and Sum are not entirely
-- unique. Any particular composition, product, or sum of functors
-- may support a variety of 'wither' implementations.

instance (Functor f, Filterable g) => Filterable (Compose f g) where
  mapMaybe f = Compose . fmap (mapMaybe f) . getCompose
  filter p = Compose . fmap (filter p) . getCompose
  catMaybes = Compose . fmap catMaybes . getCompose
  flush = Compose . fmap flush . getCompose

instance (Filterable f, Filterable g) => Filterable (P.Product f g) where
  mapMaybe f (P.Pair x y) = P.Pair (mapMaybe f x) (mapMaybe f y)
  filter p (P.Pair x y) = P.Pair (filter p x) (filter p y)
  catMaybes (P.Pair x y) = P.Pair (catMaybes x) (catMaybes y)
  flush (P.Pair x y) = P.Pair (flush x) (flush y)

instance (Filterable f, Filterable g) => Filterable (Sum.Sum f g) where
  mapMaybe f (Sum.InL x) = Sum.InL (mapMaybe f x)
  mapMaybe f (Sum.InR y) = Sum.InR (mapMaybe f y)

  catMaybes (Sum.InL x) = Sum.InL (catMaybes x)
  catMaybes (Sum.InR y) = Sum.InR (catMaybes y)

  filter p (Sum.InL x) = Sum.InL (filter p x)
  filter p (Sum.InR y) = Sum.InR (filter p y)

  flush (Sum.InL x) = Sum.InL (flush x)
  flush (Sum.InR x) = Sum.InR (flush x)

instance Filterable Generics.V1 where
  mapMaybe _ v = case v of {}
  catMaybes v = case v of {}
  filter _ v = case v of {}

instance Filterable Generics.U1 where
  mapMaybe _ _ = Generics.U1
  catMaybes _ = Generics.U1
  filter _ _ = Generics.U1

instance Filterable (Generics.K1 i c) where
  mapMaybe _ (Generics.K1 a) = Generics.K1 a
  catMaybes (Generics.K1 a) = Generics.K1 a
  filter _ (Generics.K1 a) = Generics.K1 a

instance Filterable f => Filterable (Generics.Rec1 f) where
  mapMaybe f (Generics.Rec1 a) = Generics.Rec1 (mapMaybe f a)
  catMaybes (Generics.Rec1 a) = Generics.Rec1 (catMaybes a)
  filter f (Generics.Rec1 a) = Generics.Rec1 (filter f a)

instance Filterable f => Filterable (Generics.M1 i c f) where
  mapMaybe f (Generics.M1 a) = Generics.M1 (mapMaybe f a)
  catMaybes (Generics.M1 a) = Generics.M1 (catMaybes a)
  filter f (Generics.M1 a) = Generics.M1 (filter f a)

instance (Filterable f, Filterable g) => Filterable ((Generics.:*:) f g) where
  mapMaybe f (a Generics.:*: b) = mapMaybe f a Generics.:*: mapMaybe f b
  catMaybes (a Generics.:*: b) = catMaybes a Generics.:*: catMaybes b
  filter f (a Generics.:*: b) = filter f a Generics.:*: filter f b

instance (Filterable f, Filterable g) => Filterable ((Generics.:+:) f g) where
  mapMaybe f (Generics.L1 a) = Generics.L1 (mapMaybe f a)
  mapMaybe f (Generics.R1 a) = Generics.R1 (mapMaybe f a)
  catMaybes (Generics.L1 a) = Generics.L1 (catMaybes a)
  catMaybes (Generics.R1 a) = Generics.R1 (catMaybes a)
  filter f (Generics.L1 a) = Generics.L1 (filter f a)
  filter f (Generics.R1 a) = Generics.R1 (filter f a)

instance (Functor f, Filterable g) => Filterable ((Generics.:.:) f g) where
  mapMaybe f = Generics.Comp1 . fmap (mapMaybe f) . Generics.unComp1
  catMaybes = Generics.Comp1 . fmap catMaybes . Generics.unComp1
  filter f = Generics.Comp1 . fmap (filter f) . Generics.unComp1

instance MonadPlus m => Filterable (Kleisli m a) where
  mapMaybe f (Kleisli p) = Kleisli (maybe mzero pure . f <=< p)

-- | An infix alias for 'mapMaybe'. The name of the operator alludes
-- to '<$>', and has the same fixity.
--
-- @since 0.3.1
(<$?>) :: Filterable f => (a -> Maybe b) -> f a -> f b
(<$?>) = mapMaybe
infixl 4 <$?>

-- | Flipped version of '<$?>', the 'Filterable' version of
-- 'Data.Functor.<&>'. It has the same fixity as 'Data.Functor.<&>'.
--
-- @
-- ('<&?>') = 'flip' 'mapMaybe'
-- @
--
-- @since 0.3.1
(<&?>) :: Filterable f => f a -> (a -> Maybe b) -> f b
as <&?> f = mapMaybe f as
infixl 1 <&?>
