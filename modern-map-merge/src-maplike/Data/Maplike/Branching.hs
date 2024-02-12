{-# LANGUAGE
      DeriveFunctor,
      FlexibleContexts,
      FlexibleInstances,
      GeneralizedNewtypeDeriving,
      KindSignatures,
      MultiParamTypeClasses,
      QuantifiedConstraints,
      StandaloneDeriving,
      UndecidableInstances
  #-}


-- | A variant of prefix maps, indexed on maps rather than lists.
module Data.Maplike.Branching where

import Prelude hiding (lookup, null)

import Data.Bifunctor (first)
import Data.Foldable.WithIndex
import Data.Functor.WithIndex
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Traversable.WithIndex
import Witherable

import Data.MergeTactics

import Data.Maplike


data Branching (r :: Type)
               (s :: Type)
               (m :: Type -> Type)
               (n :: Type -> Type)
               (i :: Type -> Type)
               (v :: Type) = Branching {
  content :: Maybe v,
  children :: OnPair r s m n (Branching r s m n i v)
}

instance (Maplike r i, Maplike r m, Maplike s n, Show r, Show (i s), Show v) => Show (Branching r s m n i v) where
  showsPrec = showsPrecFromFold

type MapBranching r s v = Branching r s (Map r) (Map s) (Map r) v

deriving instance (Eq v,
                   forall x. Eq x => Eq (m x),
                   forall x. Eq x => Eq (n x))
               => Eq (Branching r s m n i v)

instance (Functor m, Functor n) => Functor (Branching r s m n i) where
  fmap f (Branching h t) = Branching (f <$> h) (fmap f <$> t)

instance (Maplike r i, FunctorWithIndex r m, FunctorWithIndex s n) => FunctorWithIndex (i s) (Branching r s m n i) where
  imap f = let
    go u (Branching h t) = Branching (f u <$> h) (imap (\(k,v) -> go (insert k v u)) t)
    in go empty

instance (Foldable m, Foldable n) => Foldable (Branching r s m n i) where
  foldMap f (Branching h t) = foldMap f h <> foldMap (foldMap f) t
  foldr f z (Branching h t) = foldr f (foldr (flip (foldr f)) z t) h

instance (Maplike r i, FoldableWithIndex r m, FoldableWithIndex s n) => FoldableWithIndex (i s) (Branching r s m n i) where
  ifoldMap f = let
    go u (Branching h t) = foldMap (f u) h <> ifoldMap (\(k,v) -> go (insert k v u)) t
    in go empty
  ifoldr f = let
    go u z (Branching h t) = foldr (f u) (ifoldr (\(k,v) -> flip (go (insert k v u))) z t) h
    in go empty

instance (Traversable m, Traversable n) => Traversable (Branching r s m n i) where
  traverse f (Branching h t) = liftA2 Branching (traverse f h) (traverse (traverse f) t)

instance (Maplike r i, TraversableWithIndex r m, TraversableWithIndex s n) => TraversableWithIndex (i s) (Branching r s m n i) where
  itraverse f = let
    go u (Branching h t) = liftA2 Branching (traverse (f u) h) (itraverse (\(k,v) -> go (insert k v u)) t)
    in go empty

instance (Maplike r i, Maplike r m, Maplike s n) => Filterable (Branching r s m n i) where
  mapMaybe f (Branching h t) = Branching (mapMaybe f h) (mapMaybe (nonNull . mapMaybe f) t)

instance (Maplike r i, Maplike r m, Maplike s n) => FilterableWithIndex (i s) (Branching r s m n i) where
  imapMaybe f = let
    go u (Branching h t) = Branching (mapMaybe (f u) h) (imapMaybe (\(k,v) -> nonNull . go (insert k v u)) t)
    in go empty

instance (Maplike r i, Maplike r m, Maplike s n) => Witherable (Branching r s m n i) where
  wither f (Branching h t) = liftA2 Branching (wither f h) (wither (fmap nonNull . wither f) t)

instance (Maplike r i, Maplike r m, Maplike s n) => WitherableWithIndex (i s) (Branching r s m n i) where
  iwither f = let
    go u (Branching h t) = liftA2 Branching (wither (f u) h) (iwither (\(k,v) -> fmap nonNull . go (insert k v u)) t)
    in go empty

instance (Maplike r i, Maplike r m, Maplike s n) => Maplike (i s) (Branching r s m n i) where

  empty = Branching Nothing empty

  null (Branching h t) = null h && null t

  singleton u v = case minViewWithKey u of
    Nothing     -> Branching (Just v) empty
    Just (p,u') -> Branching Nothing . singleton p $ singleton u' v

  classify (Branching h t) = let
    u = first (const empty) (classify h)
    v = bindClassify (uncurry insert) classify (classify t)
    in u <> v

  lookup u (Branching h t) = case minViewWithKey u of
    Nothing     -> h
    Just (p,u') -> lookup u' =<< lookup p t

  alterF f u (Branching h t) = case minViewWithKey u of
    Nothing     -> flip Branching t <$> f h
    Just (p,u') -> let
      g Nothing  = fmap (singleton u') <$> f Nothing
      g (Just b) = nonNull <$> alterF f u' b
      in Branching h <$> alterF g p t

  alter f u (Branching h t) = case minViewWithKey u of
    Nothing     -> Branching (f h) t
    Just (p,u') -> let
      g Nothing  = singleton u' <$> f Nothing
      g (Just b) = nonNull $ alter f u' b
      in Branching h $ alter g p t

  insert u v (Branching h t) = case minViewWithKey u of
    Nothing -> Branching (Just v) t
    Just (p,u') -> let
      g Nothing  = singleton u' v
      g (Just b) = insert u' v b
      in Branching h $ alter (Just . g) p t

  alterMinF f (Branching h t) = let
    p Nothing  = error "alterMinF: unexpected Nothing"
    p (Just a) = nonNull <$> a
    in case h of
      Just x  -> Just (flip Branching t <$> f x)
      Nothing -> fmap (Branching h) <$> alterMinF (p . alterMinF f) t

  alterMaxF f (Branching h t) = let
    p Nothing  = error "alterMaxF: unexpected Nothing"
    p (Just a) = nonNull <$> a
    in case alterMaxF (p . alterMaxF f) t of
      Just t' -> Just (Branching h <$> t')
      Nothing -> fmap (flip Branching t) . f <$> h

  alterMinWithKeyF f = let
    p Nothing  = error "alterMinWithKeyF: unexpected Nothing"
    p (Just a) = nonNull <$> a
    go u (Branching h t) = case h of
      Just x  -> Just (flip Branching t <$> f u x)
      Nothing -> fmap (Branching h) <$> alterMinWithKeyF (\(k,v) -> p . go (insert k v u)) t
    in go empty

  alterMaxWithKeyF f = let
    p Nothing  = error "alterMaxWithKeyF: unexpected Nothing"
    p (Just a) = nonNull <$> a
    go u (Branching h t) = case alterMaxWithKeyF (\(k,v) -> p . go (insert k v u)) t of
      Just t' -> Just (Branching h <$> t')
      Nothing -> fmap (flip Branching t) . f u <$> h
    in go empty

  alterAnyWithKeyF f = let
    p Nothing  = error "alterAnyWithKeyF: unexpected Nothing"
    p (Just a) = nonNull <$> a
    go u (Branching h t) = case h of
      Just x  -> Just (flip Branching t <$> f u x)
      Nothing -> fmap (Branching h) <$> alterAnyWithKeyF (\(k,v) -> p . go (insert k v u)) t
    in go empty

  alterAnyF f (Branching h t) = let
    p Nothing  = error "alterAnyF: unexpected Nothing"
    p (Just a) = nonNull <$> a
    in case h of
      Just x  -> Just (flip Branching t <$> f x)
      Nothing -> fmap (Branching h) <$> alterAnyF (p . alterAnyF f) t

  merge l r b = let
    go (Branching h1 t1) (Branching h2 t2) = let
      onH = merge l r b
      onT = merge
              (umapMaybeMissing (nonNull . runSimpleWhenMissing (reindexMissing (const ()) l)))
              (umapMaybeMissing (nonNull . runSimpleWhenMissing (reindexMissing (const ()) r)))
              (zipWithMaybeMatched (\_ m1 m2 -> nonNull $ go m1 m2))
      in Branching (onH h1 h2) (onT t1 t2)
    in go

  mergeA l r b = let
    go (Branching h1 t1) (Branching h2 t2) = let
      onH = mergeA l r b
      onT = mergeA
              (utraverseMaybeMissing (fmap nonNull . runWhenMissing (reindexMissing (const ()) l)))
              (utraverseMaybeMissing (fmap nonNull . runWhenMissing (reindexMissing (const ()) r)))
              (WhenMatched (\_ m1 m2 -> nonNull <$> go m1 m2))
      in liftA2 Branching (onH h1 h2) (onT t1 t2)
    in go

  imerge l r b = let
    go u (Branching h1 t1) (Branching h2 t2) = let
      onH = imerge
              (reindexMissing (const u) l)
              (reindexMissing (const u) r)
              (reindexMatched (const u) b)
      onT = imerge
              (mapMaybeMissing (\(k,v) -> nonNull . runSimpleWhenMissing (reindexMissing (union (insert k v u)) l)))
              (mapMaybeMissing (\(k,v) -> nonNull . runSimpleWhenMissing (reindexMissing (union (insert k v u)) r)))
              (zipWithMaybeMatched (\(k,v) m1 m2 -> nonNull $ go (insert k v u) m1 m2))
      in Branching (onH h1 h2) (onT t1 t2)
    in go empty

  imergeA l r b = let
    go u (Branching h1 t1) (Branching h2 t2) = let
      onH = imergeA
              (reindexMissing (const u) l)
              (reindexMissing (const u) r)
              (reindexMatched (const u) b)
      onT = imergeA
              (traverseMaybeMissing (\(k,v) -> fmap nonNull . runWhenMissing (reindexMissing (union (insert k v u)) l)))
              (traverseMaybeMissing (\(k,v) -> fmap nonNull . runWhenMissing (reindexMissing (union (insert k v u)) r)))
              (WhenMatched (\(k,v) m1 m2 -> nonNull <$> go (insert k v u) m1 m2))
      in liftA2 Branching (onH h1 h2) (onT t1 t2)
    in go empty
