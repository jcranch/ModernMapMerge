{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.TraversableS where

import Data.Functor.Constant


class TraversableS k s | s -> k where
  traverseS :: Applicative f => (k -> f ()) -> s -> f s

foldMapS :: (TraversableS k s, Monoid m) => (k -> m) -> s -> m
foldMapS f = getConstant . traverseS (Constant . f)
