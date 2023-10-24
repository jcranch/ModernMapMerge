{-# LANGUAGE
      FlexibleInstances,
      MultiParamTypeClasses #-}

-- | We provide a kind of "singleton witherable" instance. This is
-- occasionally useful in applications, but is also interesting as a
-- minimal example.
module Data.IMaybe where

import Data.Functor.WithIndex
import Data.Foldable.WithIndex
import Data.Traversable.WithIndex
import Witherable


-- | An @IMaybe k v@ may consist of a key-value pair, or may not.
newtype IMaybe k v = IMaybe {
  iMaybe :: Maybe (k, v)
} deriving (Eq, Ord)

-- | A constructor of an `IMaybe` which gives a key-value pair.
iJust :: k -> v -> IMaybe k v
iJust i x = IMaybe $ Just (i,x)

-- | A constructor of an `IMaybe` which gives no key-value pair.
iNothing :: IMaybe k v
iNothing = IMaybe Nothing

instance Functor (IMaybe k) where
  fmap f (IMaybe m) = IMaybe $ fmap (fmap f) m

instance FunctorWithIndex k (IMaybe k) where
  imap f (IMaybe m) = IMaybe $ fmap (imap f) m

instance Foldable (IMaybe k) where
  foldMap f (IMaybe m) = foldMap (foldMap f) m

instance FoldableWithIndex k (IMaybe k) where
  ifoldMap f (IMaybe m) = foldMap (ifoldMap f) m

instance Traversable (IMaybe k) where
  traverse f (IMaybe m) = IMaybe <$> traverse (traverse f) m

instance TraversableWithIndex k (IMaybe k) where
  itraverse f (IMaybe m) = IMaybe <$> traverse (itraverse f) m

instance Filterable (IMaybe k) where
  mapMaybe f (IMaybe m) = IMaybe $ mapMaybe (traverse f) m

instance FilterableWithIndex k (IMaybe k) where
  imapMaybe f (IMaybe m) = IMaybe $ mapMaybe (itraverse f) m

instance Witherable (IMaybe k) where
  wither f (IMaybe m) = IMaybe <$> wither (fmap sequence . traverse f) m

instance WitherableWithIndex k (IMaybe k) where
  iwither f (IMaybe m) = IMaybe <$> wither (fmap sequence . itraverse f) m
