module Data.Map.Extra where

import Data.Map.Strict
import Data.Map.Internal


alterMinWithKeyF :: Functor f
                 => (k -> v -> f (Maybe v))
                 -> Map k v
                 -> Maybe (f (Map k v))
alterMinWithKeyF _ Tip               = Nothing
alterMinWithKeyF f (Bin s k x Tip r) = let
  g Nothing  = r
  g (Just y) = y `seq` Bin s k y Tip r
  in Just (g <$> f k x)
alterMinWithKeyF f (Bin _ k x l r)   = case alterMinWithKeyF f l of
  Nothing -> error "alterMinWithKey: balance error in tree"
  Just u  -> Just (flip (balanceR k x) r <$> u)


alterMaxWithKeyF :: Functor f
                 => (k -> v -> f (Maybe v))
                 -> Map k v
                 -> Maybe (f (Map k v))
alterMaxWithKeyF _ Tip               = Nothing
alterMaxWithKeyF f (Bin s k x l Tip) = let
  g Nothing  = l
  g (Just y) = y `seq` Bin s k y l Tip
  in Just (g <$> f k x)
alterMaxWithKeyF f (Bin _ k x l r)   = case alterMaxWithKeyF f r of
  Nothing -> error "alterMaxWithKey: balance error in tree"
  Just u  -> Just (balanceL k x l <$> u)
