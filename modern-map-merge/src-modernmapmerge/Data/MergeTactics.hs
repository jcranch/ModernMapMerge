{-# LANGUAGE
      FlexibleInstances,
      MultiParamTypeClasses,
      RankNTypes
  #-}

-- | A collection of merge tactics for the new framework.
module Data.MergeTactics where

import Control.Category (Category)
import Control.Monad ((<=<))
import Data.Foldable.WithIndex (ifoldMap)
import Data.Functor.Const
import Data.Functor.Identity
import Data.Functor.WithIndex
import Data.Traversable.WithIndex

-- This import allows us to define instances
import qualified Control.Category

import Data.Filterable.WithIndex
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
