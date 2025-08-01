{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.Maplike.Weighted where

import Data.Group
import Data.Monoid (Sum(..))


class Abelian w => Weighted a w | a -> w where
  weight :: a -> w


newtype Unweighted a = Unweighted {
  getUnweighted :: a
} deriving (Eq, Ord)

instance Weighted (Unweighted a) () where
  weight = const ()


newtype WeightOne a = WeightOne {
  getWeightOne :: a
} deriving (Eq, Ord)

instance Weighted (WeightOne a) (Sum Int) where
  weight = const (Sum 1)
