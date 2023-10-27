{-# LANGUAGE
      FlexibleInstances,
      GeneralizedNewtypeDeriving,
      MultiParamTypeClasses,
      StandaloneDeriving,
      UndecidableInstances
  #-}

module Control.Monad.Transformers.Extra where

import Control.Applicative.Backwards (Backwards (..))
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Data.Coerce (coerce)
import Data.Functor.Reverse (Reverse (..))
import qualified Data.Traversable as T

import Data.Filterable.WithIndex
import Data.Witherable.WithIndex


deriving instance Filterable f => Filterable (IdentityT f)

instance Functor f => Filterable (MaybeT f) where
  mapMaybe f = MaybeT . fmap (mapMaybe f) . runMaybeT

deriving instance Filterable t => Filterable (Reverse t)

deriving instance Filterable t => Filterable (Backwards t)

deriving instance (FilterableWithIndex i f) => FilterableWithIndex i (IdentityT f)

deriving instance FilterableWithIndex i t => FilterableWithIndex i (Reverse t)

deriving instance FilterableWithIndex i t => FilterableWithIndex i (Backwards t)

-- | Wither from right to left.
instance Witherable t => Witherable (Reverse t) where
  wither f (Reverse t) =
    fmap Reverse . forwards $ wither (coerce f) t
  -- We can't do anything special with witherM, because Backwards m is not
  -- generally a Monad.
  filterA f (Reverse t) =
    fmap Reverse . forwards $ filterA (coerce f) t

instance Witherable t => Witherable (Backwards t) where
  wither f (Backwards xs) = Backwards <$> wither f xs
  witherM f (Backwards xs) = Backwards <$> witherM f xs
  filterA f (Backwards xs) = Backwards <$> filterA f xs

instance Witherable f => Witherable (IdentityT f) where
  wither f (IdentityT m) = IdentityT <$> wither f m
  witherM f (IdentityT m) = IdentityT <$> witherM f m
  filterA p (IdentityT m) = IdentityT <$> filterA p m

instance (T.Traversable t) => Witherable (MaybeT t) where
  wither f = fmap MaybeT . T.traverse (wither f) . runMaybeT
  witherM f = fmap MaybeT . T.mapM (wither f) . runMaybeT

-- | Wither from right to left.
instance WitherableWithIndex i t => WitherableWithIndex i (Reverse t) where
  iwither f (Reverse t) = fmap Reverse . forwards $ iwither (\i -> Backwards . f i) t
  -- We can't do anything special with iwitherM, because Backwards m is not
  -- generally a Monad.
  ifilterA p (Reverse t) = fmap Reverse . forwards $ ifilterA (\i -> Backwards . p i) t

instance WitherableWithIndex i t => WitherableWithIndex i (Backwards t) where
  iwither f (Backwards xs) = Backwards <$> iwither f xs
  iwitherM f (Backwards xs) = Backwards <$> iwitherM f xs
  ifilterA f (Backwards xs) = Backwards <$> ifilterA f xs

instance (WitherableWithIndex i f) => WitherableWithIndex i (IdentityT f) where
  iwither f (IdentityT m) = IdentityT <$> iwither f m
  iwitherM f (IdentityT m) = IdentityT <$> iwitherM f m
  ifilterA p (IdentityT m) = IdentityT <$> ifilterA p m
