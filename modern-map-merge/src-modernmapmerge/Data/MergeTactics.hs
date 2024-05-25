{-# LANGUAGE
      FlexibleInstances,
      MultiParamTypeClasses,
      RankNTypes
  #-}

-- | A collection of merge tactics for the new framework.
module Data.MergeTactics where

import Prelude hiding (filter)
import Control.Category (Category)
import Control.Monad ((<=<))
import Data.Functor.Compose ()
import Data.Functor.Const
import Data.Functor.Identity
import Data.Functor.WithIndex
import Data.Foldable.WithIndex
import Data.Traversable.WithIndex

-- This import allows us to define instances
import qualified Control.Category

import Witherable
import Data.IMaybe
import Data.MergeTactics.Reindex


-- | A tactic for dealing with keys present in one map but not the
-- other in merge algorithms.
newtype WhenMissing f i x y = WhenMissing {
  runWhenMissing :: forall n. (FilterableWithIndex i n, WitherableWithIndex i n) => n x -> f (n y)
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
runSimpleWhenMissing :: (FilterableWithIndex i n, WitherableWithIndex i n) => SimpleWhenMissing i x y -> n x -> n y
runSimpleWhenMissing w = runIdentity . runWhenMissing w

-- | A variant of `missingKey` specialised to @f = Identity@.
simpleMissingKey :: SimpleWhenMissing i x y -> i -> x -> Maybe y
simpleMissingKey w i = runIdentity . missingKey w i

-- | A `WhenMissing` tactic which runs `imapMaybe`.
mapMaybeMissing :: Applicative f => (k -> x -> Maybe y) -> WhenMissing f k x y
mapMaybeMissing f = WhenMissing (pure . imapMaybe f)

-- | A keyless version of `mapMaybeMissing`
umapMaybeMissing :: Applicative f => (x -> Maybe y) -> WhenMissing f k x y
umapMaybeMissing f = WhenMissing (pure . mapMaybe f)

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
dropMissing = WhenMissing (pure . mapMaybe (const Nothing))

-- | A `WhenMissing` tactic which keeps the data as is.
preserveMissing :: Applicative f => WhenMissing f k x x
preserveMissing = WhenMissing pure

-- | A `WhenMissing` tactic which runs `imap`. For some data
-- structures this can be more efficient than `mapMaybeMissing`, since
-- the resulting data structure is known to have the same size.
mapMissing :: Applicative f => (k -> x -> y) -> WhenMissing f k x y
mapMissing f = WhenMissing (pure . imap f)

-- | A keyless version of `mapMissing`
umapMissing :: Applicative f => (x -> y) -> WhenMissing f k x y
umapMissing f = WhenMissing (pure . fmap f)

-- | A `WhenMissing` tactic which runs `ifilter`.
filterMissing :: Applicative f => (k -> x -> Bool) -> WhenMissing f k x x
filterMissing f = WhenMissing (pure . ifilter f)

-- | A keyless version of `filterMissing`
ufilterMissing :: Applicative f => (x -> Bool) -> WhenMissing f k x x
ufilterMissing f = WhenMissing (pure . filter f)

-- | The most general `WhenMissing` tactic, which runs `iwither`.
traverseMaybeMissing :: Applicative f => (k -> x -> f (Maybe y)) -> WhenMissing f k x y
traverseMaybeMissing f = WhenMissing (iwither f)

-- | A keyless version of `traverseMaybeMissing`
utraverseMaybeMissing :: Applicative f => (x -> f (Maybe y)) -> WhenMissing f k x y
utraverseMaybeMissing f = WhenMissing (wither f)

-- | A `WhenMissing` tactic which runs `itraverse`. It is possible
-- that this will be more efficient than `traverseMaybeMissing`, since
-- the resulting data structure(s) will be the same size as the
-- original.
traverseMissing :: Applicative f => (k -> x -> f y) -> WhenMissing f k x y
traverseMissing f = WhenMissing (itraverse f)

-- | A keyless version of `traverseMissing`
utraverseMissing :: Applicative f => (x -> f y) -> WhenMissing f k x y
utraverseMissing f = WhenMissing (traverse f)

-- | A `WhenMissing` tactic which runs `ifilterA`.
filterAMissing :: Applicative f => (k -> x -> f Bool) -> WhenMissing f k x x
filterAMissing f = WhenMissing (ifilterA f)

-- | A keyless version of `filterAMissing`
ufilterAMissing :: Applicative f => (x -> f Bool) -> WhenMissing f k x x
ufilterAMissing f = WhenMissing (filterA f)

-- | We can use this formalism to change the indices in a
-- `WhenMissing`.
reindexMissing :: (Functor f) => (i -> j) -> WhenMissing f j x y -> WhenMissing f i x y
reindexMissing r w = WhenMissing (fmap underlying . runWhenMissing w . Reindexed r)


-- | A tactic for keys present in both maps.
newtype WhenMatched f i x y z = WhenMatched {
  matchedKey :: i -> x -> y -> f (Maybe z)
}

reindexMatched :: (i -> j) -> WhenMatched f j x y z -> WhenMatched f i x y z
reindexMatched p (WhenMatched f) = WhenMatched (f . p)

dropMatched :: Applicative f => WhenMatched f i x y z
dropMatched = WhenMatched (\_ _ _ -> pure Nothing)

foldMatched :: (i -> x -> y -> m) -> WhenMatched (Const m) i x y z
foldMatched f = WhenMatched (\i x y -> Const $ f i x y)

ufoldMatched :: (x -> y -> m) -> WhenMatched (Const m) i x y z
ufoldMatched f = WhenMatched (\_ x y -> Const $ f x y)

zipWithMaybeAMatched :: (k -> x -> y -> f (Maybe z)) -> WhenMatched f k x y z
zipWithMaybeAMatched = WhenMatched

zipWithAMatched :: Functor f => (k -> x -> y -> f z) -> WhenMatched f k x y z
zipWithAMatched f = WhenMatched (\i x y -> Just <$> f i x y)

zipWithMaybeMatched :: Applicative f => (k -> x -> y -> Maybe z) -> WhenMatched f k x y z
zipWithMaybeMatched f = WhenMatched (\i x y -> pure $ f i x y)

zipWithMatched :: Applicative f => (k -> x -> y -> z) -> WhenMatched f k x y z
zipWithMatched f = WhenMatched (\i x y -> pure . Just $ f i x y)

-- | A keyless `zipWithMaybeAMatched`
uzipWithMaybeAMatched :: (x -> y -> f (Maybe z)) -> WhenMatched f k x y z
uzipWithMaybeAMatched = zipWithMaybeAMatched . const

-- | A keyless `zipWithAMatched`
uzipWithAMatched :: Functor f => (x -> y -> f z) -> WhenMatched f k x y z
uzipWithAMatched = zipWithAMatched . const

-- | A keyless `zipWithMaybeMatched`
uzipWithMaybeMatched :: Applicative f => (x -> y -> Maybe z) -> WhenMatched f k x y z
uzipWithMaybeMatched = zipWithMaybeMatched . const

-- | A keyless `zipWithMatched`
uzipWithMatched :: Applicative f => (x -> y -> z) -> WhenMatched f k x y z
uzipWithMatched = zipWithMatched . const

preserveLeftMatched :: Applicative f => WhenMatched f k x y x
preserveLeftMatched = WhenMatched (\_ x _ -> pure $ Just x)

preserveRightMatched :: Applicative f => WhenMatched f k x y y
preserveRightMatched = WhenMatched (\_ _ y -> pure $ Just y)

onLeftMatched :: Applicative f => WhenMissing f k x z -> WhenMatched f k x y z
onLeftMatched m = WhenMatched (\i x _ -> missingKey m i x)

onRightMatched :: Applicative f => WhenMissing f k y z -> WhenMatched f k x y z
onRightMatched m = WhenMatched (\i _ y -> missingKey m i y)


type SimpleWhenMatched = WhenMatched Identity

-- | A variant of `missingKey` specialised to @f = Identity@.
simpleMatchedKey :: SimpleWhenMatched i x y z -> i -> x -> y -> Maybe z
simpleMatchedKey w i x y = runIdentity $ matchedKey w i x y
