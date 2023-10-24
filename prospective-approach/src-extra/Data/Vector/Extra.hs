{-# LANGUAGE
      MultiParamTypeClasses
  #-}

module Data.Vector.Extra where

import Data.Functor.WithIndex.Instances ()
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Fusion.Bundle as V.B
import qualified Data.Vector.Fusion.Bundle.Size as V.BS
import qualified Data.Vector.Fusion.Bundle.Monadic as V.MB
import qualified Data.Vector.Fusion.Stream.Monadic as V.MS

import Data.Filterable.WithIndex
import Data.Witherable.WithIndex


instance Filterable V.Vector where
  filter   = V.filter
  mapMaybe = V.mapMaybe
  flush _  = V.empty

-- This is not yet in any released Vector
unstreamM :: Monad m => V.MB.Bundle m v a -> m (V.Vector a)
unstreamM s = do
  xs <- V.MB.toList s
  return $ VG.unstream $ V.B.unsafeFromList (V.MB.size s) xs
{-# INLINE unstreamM #-}

bundleWitherM :: Monad m => (a -> m (Maybe b)) -> V.MB.Bundle m v a -> V.MB.Bundle m v b
bundleWitherM g V.MB.Bundle {V.MB.sElems = s, V.MB.sSize = n} =
  V.MB.fromStream (streamWitherM g s) (V.BS.toMax n)
{-# INLINE bundleWitherM #-}

streamWitherM :: Monad m => (a -> m (Maybe b)) -> V.MS.Stream m a -> V.MS.Stream m b
streamWitherM g (V.MS.Stream step t) = V.MS.Stream step' t
  where
    {-# INLINE step' #-}
    step' s = do
      r <- step s
      case r of
        V.MS.Yield x s' -> do
          b <- g x
          case b of
            Just y  -> return $ V.MS.Yield y s'
            Nothing -> return $ V.MS.Skip    s'
        V.MS.Skip    s' -> return $ V.MS.Skip s'
        V.MS.Done       -> return V.MS.Done
{-# INLINE streamWitherM #-}

instance Witherable V.Vector where
  wither f = fmap V.fromList . wither f . V.toList
  {-# INLINABLE wither #-}

  witherM f = unstreamM . bundleWitherM f . V.B.lift . VG.stream
  {-# INLINE witherM #-}
