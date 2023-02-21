{-# LANGUAGE
      BangPatterns,
      FlexibleInstances,
      LambdaCase,
      MultiParamTypeClasses,
      RankNTypes,
      ScopedTypeVariables,
      UnboxedTuples #-}

module Data.HashMap.Extra where

import Prelude hiding (filter)

-- import Control.Applicative (liftA2)
-- import Data.Bits ((.|.))
import Data.Hashable
import qualified Data.HashMap.Lazy as HM
import Data.HashMap.Internal
-- import qualified Data.HashMap.Internal.Array as A
import Data.Functor.WithIndex.Instances ()
import Data.Traversable.WithIndex ()

import Data.Filterable.WithIndex
import Data.Witherable.WithIndex
-- import Data.MergeTactics

instance (Hashable k) => Filterable (HashMap k) where
  mapMaybe = HM.mapMaybe
  filter = HM.filter
  flush _ = HM.empty

instance (Hashable k) => FilterableWithIndex k (HashMap k) where
  imapMaybe = HM.mapMaybeWithKey
  ifilter = HM.filterWithKey

instance (Hashable k) => Witherable (HashMap k) where

instance (Hashable k) => WitherableWithIndex k (HashMap k) where

