{-# LANGUAGE
      FlexibleInstances,
      FunctionalDependencies,
      TupleSections
  #-}

-- | Pairs and compositions without UnsafeInstances, at the cost of
-- more type variables.
--
-- We use some sneaky tricks to keep the definitions short.
--
-- We don't define Filterable (or Witherable) for 'Compose' - in
-- practice, given @Compose g f@, one often wants a bespoke filter
-- which removes empty values of @f@ to stop them clogging up the @g@
-- (for example, given @'Map' j (Map i v)@, one doesn't want to store
-- empty maps as values in the outer map).
module Data.Functor.Extra where

import Prelude hiding (filter)

import Control.Applicative (liftA2)
import Data.Functor.WithIndex
import Data.Foldable.WithIndex
import Data.Traversable.WithIndex
import Data.Filterable.WithIndex
import Data.Witherable.WithIndex


newtype ICompose j i g f a = ICompose {
  getICompose :: g (f a)
} deriving (Eq, Ord)

instance (Functor g, Functor f) => Functor (ICompose j i g f) where
  fmap f (ICompose x) = ICompose (fmap (fmap f) x)

instance (FunctorWithIndex j g, FunctorWithIndex i f) => FunctorWithIndex (j,i) (ICompose j i g f) where
  imap f (ICompose x) = ICompose (imap (imap . curry f) x)

instance (Foldable g, Foldable f) => Foldable (ICompose j i g f) where
  foldl f a (ICompose x) = foldl (foldl f) a x
  foldr f a (ICompose x) = foldr (flip (foldr f)) a x
  foldMap f (ICompose x) = foldMap (foldMap f) x

instance (FoldableWithIndex j g, FoldableWithIndex i f) => FoldableWithIndex (j,i) (ICompose j i g f) where
  ifoldl f a (ICompose x) = ifoldl (ifoldl . curry f) a x
  ifoldr f a (ICompose x) = ifoldr (flip . ifoldr . curry f) a x
  ifoldMap f (ICompose x) = ifoldMap (ifoldMap . curry f) x

instance (Traversable g, Traversable f) => Traversable (ICompose j i g f) where
  traverse f (ICompose x) = ICompose <$> traverse (traverse f) x

instance (TraversableWithIndex j g, TraversableWithIndex i f) => TraversableWithIndex (j,i) (ICompose j i g f) where
  itraverse f (ICompose x) = ICompose <$> itraverse (itraverse . curry f) x



data IProduct i j f g a = IPair {
  ifst :: f a,
  isnd :: g a
} deriving (Eq, Ord)

iswap :: IProduct i j f g a -> IProduct j i g f a
iswap (IPair u v) = IPair v u

instance (Functor f, Functor g) => Functor (IProduct i j f g) where
  fmap f (IPair u v) = IPair (fmap f u) (fmap f v)

instance (FunctorWithIndex i f, FunctorWithIndex j g) => FunctorWithIndex (Either i j) (IProduct i j f g) where
  imap f (IPair u v) = IPair (imap (f . Left) u) (imap (f . Right) v)

instance (Foldable f, Foldable g) => Foldable (IProduct i j f g) where
  foldl f x (IPair u v) = foldl f (foldl f x u) v
  foldr f x (IPair u v) = foldr f (foldr f x v) u
  foldMap f (IPair u v) = foldMap f u <> foldMap f v

instance (FoldableWithIndex i f, FoldableWithIndex j g) => FoldableWithIndex (Either i j) (IProduct i j f g) where
  ifoldl f x (IPair u v) = ifoldl (f . Right) (ifoldl (f . Left) x u) v
  ifoldr f x (IPair u v) = ifoldr (f . Left) (ifoldr (f . Right) x v) u
  ifoldMap f (IPair u v) = ifoldMap (f . Left) u <> ifoldMap (f . Right) v

instance (Traversable f, Traversable g) => Traversable (IProduct i j f g) where
  traverse f (IPair u v) = liftA2 IPair (traverse f u) (traverse f v)

instance (TraversableWithIndex i f, TraversableWithIndex j g) => TraversableWithIndex (Either i j) (IProduct i j f g) where
  itraverse f (IPair u v) = liftA2 IPair (itraverse (f . Left) u) (itraverse (f . Right) v)

instance (Filterable f, Filterable g) => Filterable (IProduct i j f g) where
  mapMaybe f (IPair u v) = IPair (mapMaybe f u) (mapMaybe f v)
  filter f (IPair u v) = IPair (filter f u) (filter f v)

instance (FilterableWithIndex i f, FilterableWithIndex j g) => FilterableWithIndex (Either i j) (IProduct i j f g) where
  imapMaybe f (IPair u v) = IPair (imapMaybe (f . Left) u) (imapMaybe (f . Right) v)
  ifilter f (IPair u v) = IPair (ifilter (f . Left) u) (ifilter (f . Right) v)

instance (Witherable f, Witherable g) => Witherable (IProduct i j f g) where
  wither f (IPair u v) = liftA2 IPair (wither f u) (wither f v)
  witherM f (IPair u v) = liftA2 IPair (witherM f u) (witherM f v)
  filterA f (IPair u v) = liftA2 IPair (filterA f u) (filterA f v)

instance (WitherableWithIndex i f, WitherableWithIndex j g) => WitherableWithIndex (Either i j) (IProduct i j f g) where
  iwither f (IPair u v) = liftA2 IPair (iwither (f . Left) u) (iwither (f . Right) v)
  iwitherM f (IPair u v) = liftA2 IPair (iwitherM (f . Left) u) (iwitherM (f . Right) v)
  ifilterA f (IPair u v) = liftA2 IPair (ifilterA (f . Left) u) (ifilterA (f . Right) v)
