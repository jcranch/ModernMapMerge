{-# LANGUAGE DeriveFunctor #-}

-- | A data structure for representing an empty container, a singleton
-- container, or a container with more elements
module Data.Classified where

import Data.Bifunctor (Bifunctor(..))


-- | We can be interested in whether a data structure consists of no
-- elements, one element or more than that
data Classified k v = Zero | One k v | Many
  deriving (Eq, Ord, Functor)

instance Semigroup (Classified k v) where
  Zero    <> a    = a
  One k v <> Zero = One k v
  One _ _ <> _    = Many
  Many    <> _    = Many

instance Monoid (Classified k v) where
  mempty = Zero

instance Bifunctor Classified where
  first _ Zero = Zero
  first _ Many = Many
  first f (One k v) = One (f k) v
  bimap _ _ Zero      = Zero
  bimap _ _ Many      = Many
  bimap f g (One k v) = One (f k) (g v)

-- | I think this is some kind of indexed monadic functionality
bindClassify :: (i -> j -> k) -> (u -> Classified j v) -> Classified i u -> Classified k v
bindClassify p f m = case m of
  Zero    -> Zero
  Many    -> Many
  One k n -> first (p k) (f n)
