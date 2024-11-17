{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.WitherableS where

import Data.FilterableS
import Data.TraversableS


class (FilterableS k s, TraversableS k s) => Witherable k s | s -> k where
  witherS :: Applicative f => (k -> f Bool) -> s -> f s
