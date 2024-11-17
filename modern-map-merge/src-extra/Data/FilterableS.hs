{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.FilterableS where


class FilterableS k s | s -> k where
  filterS :: (k -> Bool) -> s -> s
