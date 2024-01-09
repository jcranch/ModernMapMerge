module Data.Map.Extra where

import Data.Map.Strict
import Data.Map.Internal


alterMinWithKeyA :: Functor f
                 => (k -> v -> f (Maybe v))
                 -> Map k v
                 -> Maybe (f (Map k v))
alterMinWithKeyA _ Tip               = Nothing
alterMinWithKeyA f (Bin s k x Tip r) = let
  g Nothing  = r
  g (Just y) = y `seq` Bin s k y Tip r
  in Just (g <$> f k x)
alterMinWithKeyA f (Bin _ k x l r)   = case alterMinWithKeyA f l of
  Nothing -> error "alterMinWithKey: balance error in tree"
  Just u  -> Just (flip (balanceR k x) r <$> u)


alterMaxWithKeyA :: Functor f
                 => (k -> v -> f (Maybe v))
                 -> Map k v
                 -> Maybe (f (Map k v))
alterMaxWithKeyA _ Tip               = Nothing
alterMaxWithKeyA f (Bin s k x l Tip) = let
  g Nothing  = l
  g (Just y) = y `seq` Bin s k y l Tip
  in Just (g <$> f k x)
alterMaxWithKeyA f (Bin _ k x l r)   = case alterMaxWithKeyA f r of
  Nothing -> error "alterMaxWithKey: balance error in tree"
  Just u  -> Just (balanceL k x l <$> u)
