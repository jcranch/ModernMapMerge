{-# LANGUAGE
      MultiParamTypeClasses
  #-}


module Data.Radix1Tree.Word8.Lazy.Merge where

import Data.Maplike
import Data.Radix1Tree.Word8.Key (Build1)
import Data.Radix1Tree.Word8.Lazy.Unsafe (Radix1Tree(..))


instance Maplike Build1 Radix1Tree where
