{-# LANGUAGE BangPatterns #-}

module Data.IntMap.Extra where

import Data.IntMap.Strict
import Data.IntMap.Internal


alterMinWithKeyF :: Functor f
                 => (Int -> v -> f (Maybe v))
                 -> IntMap v
                 -> Maybe (f (IntMap v))
alterMinWithKeyF f t = let
  go (Bin p m l r) = flip (binCheckLeft p m) r <$> go l
  go (Tip k y)     = let
    g (Just !z) = Tip k z
    g Nothing   = Nil
    in g <$> f k y
  go Nil           = error "alterMinWithKeyF: unexpected Nil"
  in case t of
    Nil                 -> Nothing
    Bin p m l r | m < 0 -> Just (binCheckRight p m l <$> go r)
    _                   -> Just (go t)



alterMaxWithKeyF :: Functor f
                 => (Int -> v -> f (Maybe v))
                 -> IntMap v
                 -> Maybe (f (IntMap v))
alterMaxWithKeyF f t = let
  go (Bin p m l r) = binCheckRight p m l <$> go r
  go (Tip k y)     = let
    g (Just !z) = Tip k z
    g Nothing   = Nil
    in g <$> f k y
  go Nil           = error "alterMaxWithKeyF: unexpected Nil"
  in case t of
    Nil                 -> Nothing
    Bin p m l r | m < 0 -> Just (flip (binCheckLeft p m) r <$> go l)
    _                   -> Just (go t)

