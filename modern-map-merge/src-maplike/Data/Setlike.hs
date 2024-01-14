{-# LANGUAGE
      FlexibleContexts,
      FlexibleInstances,
      TupleSections #-}

-- | If n is like a map type, then n () is like a set type.

module Data.Setlike where

import Prelude hiding (null)

import Data.Foldable.WithIndex (ifoldr)
import Data.Maybe (isJust)

import Data.Maplike (Maplike)
import qualified Data.Maplike as M


newtype Set i n = Set { setMap :: n () }

empty :: Maplike i n => Set i n
empty = Set M.empty

null :: Maplike i n => Set i n -> Bool
null (Set m) = M.null m

singleton :: Maplike i n => i -> Set i n
singleton x = Set $ M.singleton x ()

member :: Maplike i n => i -> Set i n -> Bool
member x (Set s) = isJust $ M.lookup x s

setFoldr :: Maplike i n => (i -> a -> a) -> a -> Set i n -> a
setFoldr f x (Set m) = ifoldr (\i _ -> f i) x m

insert :: Maplike i n => i -> Set i n -> Set i n
insert x (Set m) = Set $ M.insert x () m

delete :: Maplike i n => i -> Set i n -> Set i n
delete x (Set m) = Set $ M.delete x m

union :: Maplike i n => Set i n -> Set i n -> Set i n
union (Set m) (Set n) = Set (M.union m n)

intersection :: Maplike i n => Set i n -> Set i n -> Set i n
intersection (Set m) (Set n) = Set (M.intersection m n)

difference :: Maplike i n => Set i n -> Set i n -> Set i n
difference (Set m) (Set n) = Set (M.difference m n)

toList :: Maplike i n => Set i n -> [i]
toList = setFoldr (:) []

fromFold :: (Foldable f, Maplike i n) => f i -> Set i n
fromFold = foldr insert empty
