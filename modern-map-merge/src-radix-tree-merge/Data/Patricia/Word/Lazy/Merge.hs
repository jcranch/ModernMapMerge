{-# LANGUAGE
      MultiParamTypeClasses
  #-}

module Data.Patricia.Word.Lazy.Merge where

import Data.Foldable.WithIndex
import Data.Functor.WithIndex
import Data.Traversable.WithIndex
import Data.Maplike
import Data.Patricia.Word.Lazy.Unsafe (Patricia(..), Prefix)
import qualified Data.Patricia.Word.Lazy as P
import Witherable

-- This function is important but inaccessible :-(

{-# INLINE rebin #-}
-- | Reconstruct a 'Bin' knowing that either of the sides may now be a 'Nil'.
rebin :: Prefix -> Patricia a -> Patricia a -> Patricia a
rebin _ Nil r   = r
rebin _ l   Nil = l
rebin p l   r   = Bin p l r

instance Filterable Patricia where
  mapMaybe = P.mapMaybe

instance Witherable Patricia where
  wither f = let
    go t = case t of
      Bin p l r -> liftA2 (rebin p) (go l) (go r)
      Tip k a -> let
        g (Just b) = Tip k b
        g Nothing  = Nil
        in g <$> f a
      Nil -> pure Nil
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
      Tip k a -> let
        g (Just b) = Tip k b
        g Nothing  = Nil
        in g <$> f k a
      Nil -> pure Nil
    in go

instance Maplike Word Patricia where

  empty = P.empty

  null = P.null

  singleton = P.singleton

{-
{-# INLINE merge #-}
-- | \(\mathcal{O}(1)\texttt{+}, \mathcal{O}(n_A + n_B)\).
--   General merge of two trees.
--
--   Collision and single value functions __must__ return either
--   'Tip' with the respective key, or 'Nil'.
--
--   Subtree argument functions may return any tree, however the shape of said tree
--   __must__ be compatible with the prefix passed to the function.
--
--   This functions inlines when all argument functions are provided.
merge
  :: (Key -> a -> b -> Patricia c)                      -- ^ Collision
  -> (Key -> a -> Patricia c)                           -- ^ Single left value
  -> (Prefix -> Patricia a -> Patricia a -> Patricia c) -- ^ Left subtree
  -> (Key -> b -> Patricia c)                           -- ^ Single right value
  -> (Prefix -> Patricia b -> Patricia b -> Patricia c) -- ^ Right subtree
  -> Patricia a
  -> Patricia b
  -> Patricia c
merge (f :: Key -> x -> y -> Patricia c) oneX treeX oneY treeY = anyAny L
  where
    {-# INLINE side #-}
    side one tree t =
      case t of
        Bin p l r -> tree p l r
        Tip k a   -> one k a
        Nil       -> Nil

    sideX = side oneX treeX

    sideY = side oneY treeY

    sideA :: forall a b. S a b x y -> Patricia a -> Patricia c
    sideA s tA = case s of
                   L -> sideX tA
                   R -> sideY tA

    sideB :: forall a b. S a b x y -> Patricia b -> Patricia c
    sideB s tB = case s of
                   L -> sideY tB
                   R -> sideX tB

    anyAny
      :: forall a b. S a b x y -> Patricia a -> Patricia b -> Patricia c
    anyAny s tA tB =
      case tA of
        Bin pA lA rA -> binAny s (# pA, lA, rA #) tA tB

        Tip kA a     -> tipAny s (# kA, a #) tA tB

        Nil          -> sideB s tB

    tipAny
      :: forall a b. S a b x y -> UTip a -> Patricia a -> Patricia b -> Patricia c
    tipAny s uA@(# kA, a #) tA tB =
      case tB of
        Bin pB lB rB -> tipBin s uA tA (# pB, lB, rB #)

        Tip kB b
          | kA == kB  -> case s of
                           L -> f kA a b
                           R -> f kA b a

          | otherwise -> case s of
                           L -> safeJoin kA (oneX kA a) kB (sideY tB)
                           R -> safeJoin kA (oneY kA a) kB (sideX tB)

        Nil          -> sideA s tA

    binAny
      :: forall a b. S a b x y -> UBin a -> Patricia a -> Patricia b -> Patricia c
    binAny s uA tA tB =
      case tB of
        Bin pB lB rB -> binBin s uA tA (# pB, lB, rB #) tB

        Tip kB b     -> let !(# s' #) = other s
                        in tipBin s' (# kB, b #) tB uA

        Nil          -> sideA s tA

    tipBin
      :: forall a b. S a b x y -> UTip a -> Patricia a -> UBin b -> Patricia c
    tipBin s uA@(# kA, a #) tA (# pB, lB, rB #)
      | beyond pB kA = case s of
                         L -> safeJoin kA (oneX kA a) pB (treeY pB lB rB)
                         R -> safeJoin kA (oneY kA a) pB (treeX pB lB rB)

      | kA < pB      = rebin pB (tipAny s uA tA lB) (sideB s rB)

      | otherwise    = rebin pB (sideB s lB) (tipAny s uA tA rB)

    binBin
      :: forall a b. S a b x y -> UBin a -> Patricia a -> UBin b -> Patricia b -> Patricia c
    binBin s uA@(# pA, lA, rA #) tA uB@(# pB, lB, rB #) tB =
      let {-# NOINLINE no #-}
          no = case s of
                 L -> safeJoin pA (treeX pA lA rA) pB (treeY pB lB rB)
                 R -> safeJoin pA (treeY pA lA rA) pB (treeX pB lB rB)

      in case Prelude.compare pA pB of
           EQ                  -> rebin pA (anyAny s lA lB) (anyAny s rA rB)

           LT | pB <= upper pA -> let !(# s' #) = other s

                                  in rebin pA (sideA s lA) (binAny s' uB tB rA)

              | pA >= lower pB -> rebin pB (binAny s uA tA lB) (sideB s rB)

              | otherwise      -> no

           GT | pA <= upper pB -> rebin pB (sideB s lB) (binAny s uA tA rB)

              | pB >= lower pA -> let !(# s' #) = other s

                                  in rebin pA (binAny s' uB tB lA) (sideA s rA)

              | otherwise      -> no
-}
