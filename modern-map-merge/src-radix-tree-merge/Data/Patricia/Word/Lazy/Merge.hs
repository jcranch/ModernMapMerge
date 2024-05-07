{-# LANGUAGE
      BangPatterns,
      ExistentialQuantification,
      GADTs,
      MultiParamTypeClasses,
      RankNTypes,
      UnboxedTuples
  #-}

module Data.Patricia.Word.Lazy.Merge where

import Data.Bits ((.|.))
import Data.Foldable.WithIndex
import Data.Functor.WithIndex
import Data.Traversable.WithIndex
import Data.Maplike
import Data.MergeTactics
import Data.Patricia.Word.Lazy.Unsafe (Patricia(..), Prefix)
import qualified Data.Patricia.Word.Lazy as P
import qualified Data.Patricia.Word.Lazy.Unsafe as P
import Witherable

-- This functionality is important but inaccessible :-(

{-# INLINE rebin #-}
-- | Reconstruct a 'Bin' knowing that either of the sides may now be a 'Nil'.
rebin :: Prefix -> Patricia a -> Patricia a -> Patricia a
rebin _ Nil r   = r
rebin _ l   Nil = l
rebin p l   r   = Bin p l r

{-# INLINE rebinL #-}
-- | Reconstruct a 'Bin' knowing that the left side may now be a 'Nil'.
rebinL :: Prefix -> Patricia a -> Patricia a -> Patricia a
rebinL p l r =
  case l of
    Nil -> r
    _   -> Bin p l r

{-# INLINE rebinR #-}
-- | Reconstruct a 'Bin' knowing that the right side may now be a 'Nil'.
rebinR :: Prefix -> Patricia a -> Patricia a -> Patricia a
rebinR p l r =
  case r of
    Nil -> l
    _   -> Bin p l r


{-# INLINE safeJoin #-}
-- | Knowing that the prefixes of two trees disagree, construct a 'Bin'.
safeJoin :: Prefix -> Patricia a -> Prefix -> Patricia a -> Patricia a
safeJoin _  Nil _  t1  = t1
safeJoin _  t0  _  Nil = t0
safeJoin p0 t0  p1 t1  = join p0 t0 p1 t1

{-# INLINE join #-}
-- | Knowing that the prefixes of two non-'Nil' trees disagree, construct a 'Bin'.
join :: Prefix -> Patricia a -> Prefix -> Patricia a -> Patricia a
join p0 t0 p1 t1 =
  let m = P.branchingBit p0 p1
      p = P.mask p0 m .|. m
  in if P.zeroBit p0 m
       then Bin p t0 t1
       else Bin p t1 t0

type UBin a = (# Prefix, Patricia a, Patricia a #)

type UTip a = (# Word, a #)

-- | Merge side.
data S a b x y where
  L :: S x y x y
  R :: S y x x y

-- | The other merge side.
other :: S a b x y -> (# S b a x y #)
other L = (# R #)
other R = (# L #)



instance Filterable Patricia where
  mapMaybe = P.mapMaybe

instance Witherable Patricia where
  wither f = let
    go t = case t of
      Bin p l r -> liftA2 (rebin p) (go l) (go r)
      Tip k a   -> maybeSingleton k <$> f a
      Nil       -> pure Nil
    in go

instance FunctorWithIndex Word Patricia where
  imap = P.mapWithKey

instance FoldableWithIndex Word Patricia where
  ifoldMap = P.foldMapWithKey

instance TraversableWithIndex Word Patricia where
  itraverse = P.traverseWithKey
  
instance FilterableWithIndex Word Patricia where
  imapMaybe = P.mapMaybeWithKey

instance WitherableWithIndex Word Patricia where
  iwither f = let
    go t = case t of
      Bin p l r -> liftA2 (rebin p) (go l) (go r)
      Tip k a   -> maybeSingleton k <$> f k a
      Nil       -> pure Nil
    in go

instance Maplike Word Patricia where

  empty = P.empty

  null = P.null

  singleton = P.singleton

  alterF f !w = let
    go t =
      case t of
        Bin p l r
          | P.beyond p w -> let
              g (Just b) = join p t w (Tip w b)
              g Nothing  = t
              in g <$> f Nothing
          | w < p      -> flip (rebinL p) r <$> go l
          | otherwise  -> rebinR p l <$> go r

        Tip k a
          | k == w -> maybeSingleton k <$> f (Just a)
          | otherwise -> let
              g (Just b) = join k t w (Tip w b)
              g Nothing  = t
              in g <$> f Nothing

        Nil -> maybeSingleton w <$> f Nothing
    in go

  alterMinWithKeyF f = let
    go t = case t of
      Bin p l r -> fmap (flip (rebinL p) r) <$> go l
      Tip k a   -> Just (maybeSingleton k <$> f k a)
      Nil       -> Nothing
    in go

  alterMaxWithKeyF f = let
    go t = case t of
      Bin p l r -> fmap (rebinR p l) <$> go r
      Tip k a   -> Just (maybeSingleton k <$> f k a)
      Nil       -> Nothing
    in go

  imergeA onL onR onB = let

    sideX = runWhenMissing onL
    sideY = runWhenMissing onR

    sideA s tA = case s of
                   L -> sideX tA
                   R -> sideY tA

    sideB s tB = case s of
                   L -> sideY tB
                   R -> sideX tB

    anyAny s tA tB =
      case tA of
        Bin pA lA rA -> binAny s (# pA, lA, rA #) tA tB
        Tip kA a     -> tipAny s (# kA, a #) tA tB
        Nil          -> sideB s tB

    tipAny s uA@(# kA, a #) tA tB =
      case tB of
        Bin pB lB rB -> tipBin s uA tA (# pB, lB, rB #)

        Tip kB b
          | kA == kB  -> case s of
                           L -> matchedKey onB kA a b
                           R -> matchedKey onB kA b a

          | otherwise -> case s of
                           L -> liftA2 (flip (safeJoin kA) kB) (missingKey onL kA a) (sideY tB)
                           R -> liftA2 (flip (safeJoin kA) kB) (missingKey onR kA a) (sideX tB)

        Nil          -> sideA s tA

    binAny s uA tA tB =
      case tB of
        Bin pB lB rB -> binBin s uA tA (# pB, lB, rB #) tB

        Tip kB b     -> let !(# s' #) = other s
                        in tipBin s' (# kB, b #) tB uA

        Nil          -> sideA s tA

    tipBin s uA@(# kA, a #) tA (# pB, lB, rB #)
      | P.beyond pB kA = case s of
                           L -> liftA2 (flip (safeJoin kA) pB) (missingKey onL kA a) (runWhenMissing onR $ Bin pB lB rB)
                           R -> liftA2 (flip (safeJoin kA) pB) (missingKey onR kA a) (runWhenMissing onL $ Bin pB lB rB)

      | kA < pB      = rebin pB (tipAny s uA tA lB) (sideB s rB)

      | otherwise    = rebin pB (sideB s lB) (tipAny s uA tA rB)

    binBin s uA@(# pA, lA, rA #) tA uB@(# pB, lB, rB #) tB =
      let {-# NOINLINE no #-}
          no = case s of
                 L -> liftA2 (flip (safeJoin pA) pB) (runWhenMissing onL $ Bin pA lA rA) (runWhenMissing onR $ Bin pB lB rB)
                 R -> liftA2 (flip (safeJoin pA) pB) (runWhenMissing onR $ Bin pA lA rA) (runWhenMissing onL $ Bin pB lB rB)

      in case Prelude.compare pA pB of
           EQ                    -> liftA2 (rebin pA) (anyAny s lA lB) (anyAny s rA rB)

           LT | pB <= P.upper pA -> let !(# s' #) = other s

                                    in liftA2 (rebin pA) (sideA s lA) (binAny s' uB tB rA)

              | pA >= P.lower pB -> liftA2 (rebin pB) (binAny s uA tA lB) (sideB s rB)

              | otherwise      -> no

           GT | pA <= P.upper pB -> liftA2 (rebin pB) (sideB s lB) (binAny s uA tA rB)

              | pB >= P.lower pA -> let !(# s' #) = other s

                                    in liftA2 (rebin pA) (binAny s' uB tB lA) (sideA s rA)

              | otherwise      -> no
        
    in anyAny L
