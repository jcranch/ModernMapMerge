{-# LANGUAGE
      FlexibleInstances,
      MultiParamTypeClasses,
      RankNTypes
  #-}

-- | A collection of merge tactics for the new framework.
module Data.MergeTactics where

import Control.Category (Category)
import Control.Monad ((<=<))
import Data.Functor.Compose ()
import Data.Functor.Const
import Data.Functor.Identity
import Data.Functor.WithIndex

-- This import allows us to define instances
import qualified Control.Category

import Data.Witherable.WithIndex
import Data.IMaybe


-- | A tactic for dealing with keys present in one map but not the
-- other in merge algorithms.
newtype WhenMissing f i x y = WhenMissing {
  runWhenMissing :: forall n. WitherableWithIndex i n => n x -> f (n y)
}

-- | Run a WhenMissing tactic on a single key
missingKey :: Functor f => WhenMissing f i x y -> i -> x -> f (Maybe y)
missingKey (WhenMissing w) i x = fmap snd . iMaybe <$> w (iJust i x)

instance (Functor f)
      => Functor (WhenMissing f i x) where
  fmap f (WhenMissing w) = WhenMissing (fmap (fmap f) . w)

instance (FunctorWithIndex i f)
      => FunctorWithIndex i (WhenMissing f i x) where
  imap f (WhenMissing w) = WhenMissing (fmap (imap f) . w)

instance (Filterable f)
      => Filterable (WhenMissing f i x) where
  mapMaybe f (WhenMissing w) = WhenMissing (fmap (mapMaybe f) . w)

instance (FilterableWithIndex i f)
      => FilterableWithIndex i (WhenMissing f i x) where
  imapMaybe f (WhenMissing w) = WhenMissing (fmap (imapMaybe f) . w)

instance (Monad f)
      => Category (WhenMissing f i) where
  id = WhenMissing pure
  WhenMissing w . WhenMissing v = WhenMissing (w <=< v)


-- | Merge algorithms can be valued in an arbitrary applicative
-- functor @f@. The commonest and simplest use case is when that
-- functor is `Identity`; we provide variants specialised to that
-- case.
type SimpleWhenMissing = WhenMissing Identity

-- | A variant of `runWhenMissing` specialised to @f = Identity@.
runSimpleWhenMissing :: (WitherableWithIndex i n) => SimpleWhenMissing i x y -> n x -> n y
runSimpleWhenMissing w = runIdentity . runWhenMissing w

-- | A variant of `missingKey` specialised to @f = Identity@.
simpleMissingKey :: SimpleWhenMissing i x y -> i -> x -> Maybe y
simpleMissingKey w i = runIdentity . missingKey w i

-- | A `WhenMissing` tactic which runs `imapMaybe`.
mapMaybeMissing :: Applicative f => (k -> x -> Maybe y) -> WhenMissing f k x y
mapMaybeMissing f = WhenMissing (pure . imapMaybe f)

-- | A `WhenMissing` tactic which folds (using a key), specialised to
-- $f = Const m$.
foldMissingKey :: Monoid m => (k -> x -> m) -> WhenMissing (Const m) k x y
foldMissingKey f = WhenMissing (Const . ifoldMap f)

-- | A `WhenMissing` tactic which folds, specialised to $f = Const m$.
foldMissing :: Monoid m => (x -> m) -> WhenMissing (Const m) k x y
foldMissing f = WhenMissing (Const . foldMap f)

-- | A `WhenMissing` tactic which drops all elements. This is often
-- rapid: often consisting of returning an empty data structure.
dropMissing :: Applicative f => WhenMissing f k x y
dropMissing = WhenMissing (pure . flush)

-- | A `WhenMissing` tactic which keeps the data as is.
preserveMissing :: Applicative f => WhenMissing f k x x
preserveMissing = WhenMissing pure

-- | A `WhenMissing` tactic which runs `imap`. For some data
-- structures this can be more efficient than `mapMaybeMissing`, since
-- the resulting data structure is known to have the same size.
mapMissing :: Applicative f => (k -> x -> y) -> WhenMissing f k x y
mapMissing f = WhenMissing (pure . imap f)

-- | A `WhenMissing` tactic which runs `ifilter`.
filterMissing :: Applicative f => (k -> x -> Bool) -> WhenMissing f k x x
filterMissing f = WhenMissing (pure . ifilter f)

-- | The most general `WhenMissing` tactic, which runs `iwither`.
traverseMaybeMissing :: Applicative f => (k -> x -> f (Maybe y)) -> WhenMissing f k x y
traverseMaybeMissing f = WhenMissing (iwither f)

-- | A `WhenMissing` tactic which runs `itraverse`. It is possible
-- that this will be more efficient than `traverseMaybeMissing`, since
-- the resulting data structure(s) will be the same size as the
-- original.
traverseMissing :: Applicative f => (k -> x -> f y) -> WhenMissing f k x y
traverseMissing f = WhenMissing (itraverse f)

-- | A `WhenMissing` tactic which runs `ifilterA`.
filterAMissing :: Applicative f => (k -> x -> f Bool) -> WhenMissing f k x x
filterAMissing f = WhenMissing (ifilterA f)


-- | A tactic for keys present in both maps.
newtype WhenMatched f i x y z = WhenMatched {
  matchedKey :: i -> x -> y -> f (Maybe z)
}

-- Functor . Functor
{-# RULES
  "fmap.fmap"           forall f g a.  fmap g (fmap f a) = fmap (g . f) a
  #-}

-- Functor . FunctorWithIndex
{-# RULES
  "fmap.imap"           forall f g a.  fmap g (imap f a) = imap (\i -> g . f i) a
  #-}

-- Functor . Traversable
{-# RULES
  "fmap.traverse"       forall f g a.  fmap (fmap g) (traverse f a) = traverse (fmap g . f) a
  #-}

-- Functor . TraversableWithIndex
{-# RULES
  "fmap.itraverse"      forall f g a.  fmap (fmap g) (itraverse f a) = itraverse (\i -> fmap g . f i) a
  #-}

-- Functor . Filterable
{-# RULES
  "fmap.filter"         forall f g a.  fmap g (filter f a) = mapMaybe (\x -> if f x then Just (g x) else Nothing) a
  "fmap.mapMaybe"       forall f g a.  fmap g (mapMaybe f a) = mapMaybe (fmap g . f) a
  "fmap.catMaybes"      forall g a.    fmap g (catMaybes a) = mapMaybe (fmap g) a
  #-}

-- Foldable . Functor 
{-# RULES
  "foldMap.fmap"        forall f g a.  foldMap g (fmap f a) = foldMap (g . f) a
  #-}

-- FunctorWithIndex . Functor
{-# RULES
  "imap.fmap"           forall f g a.  imap g (fmap f a) = imap (\i -> g i . f) a
  #-}

-- FunctorWithIndex . FunctorWithIndex
{-# RULES
  "imap.imap"           forall f g a.  imap g (imap f a) = imap (\i -> g i . f i) a
  #-}

-- Traversable . Traversable
{-# RULES
  "traverse.traverse"   forall f g a.  fmap (traverse g) (traverse f a) = getCompose (traverse (Compose . fmap g . f) a)
  #-}

-- Filterable . Functor
{-# RULES
  "filter.fmap"         forall f g a.  filter g (fmap f a) = mapMaybe ((\x -> if g x then Just x else Nothing) . f) a
  "mapMaybe.fmap"       forall f g a.  mapMaybe g (fmap f a) = mapMaybe (g . f) a
  "catMaybes.fmap"      forall f a.    catMaybes (fmap f a) = mapMaybe f a
  #-}

-- Filterable . Filterable
{-# RULES
  "filter.filter"       forall f g a.  filter g (filter f a) = filter (liftA2 (&&) f g) a
  "filter.mapMaybe"     forall f g a.  filter g (mapMaybe f a) = _
  "mapMaybe.mapMaybe"   forall f g a.  mapMaybe g (mapMaybe f a) = _
  "mapMaybe.filter"     forall f g a.  mapMaybe g (filter f a) = _
  #-}

-- Witherable . Witherable

-- WitherableWithIndex . WitherableWithIndex
{-# RULES
  "iwither.iwither"     forall f g a.  fmap (iwither g) . iwither f = getCompose . iwither (Compose . _)
  #-}
