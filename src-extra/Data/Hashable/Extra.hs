{-# LANGUAGE
      DeriveFunctor
  #-}

module Data.Hashable.Extra where

import Control.Monad.Trans.State.Lazy (evalState, state)
import Data.Hashable
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.Set as S

import Data.Witherable


-- | Removes duplicate elements from a list, keeping only the first
-- occurrence. This is asymptotically faster than using
-- 'Data.List.nub' from "Data.List".
--
-- >>> ordNub [3,2,1,3,2,1]
-- [3,2,1]
--
ordNub :: (Witherable t, Ord a) => t a -> t a
ordNub = ordNubOn id
{-# INLINE ordNub #-}

-- | The 'ordNubOn' function behaves just like 'ordNub', except it
-- uses another type to determine equivalence classes.
--
-- >>> ordNubOn fst [(True, 'x'), (False, 'y'), (True, 'z')]
-- [(True,'x'),(False,'y')]
--
ordNubOn :: (Witherable t, Ord b) => (a -> b) -> t a -> t a
ordNubOn p t = evalState (witherM f t) S.empty where
    f a = state $ \s ->
      case S.alterF (\x -> BoolPair x True) (p a) s of
        BoolPair True  s' -> (Nothing, s')
        BoolPair False s' -> (Just a,  s')
{-# INLINE ordNubOn #-}

-- | Removes duplicate elements from a list, keeping only the first
--   occurrence. This is usually faster than 'ordNub', especially for
--   things that have a slow comparison (like 'String').
--
-- >>> hashNub [3,2,1,3,2,1]
-- [3,2,1]
--
hashNub :: (Witherable t, Hashable a) => t a -> t a
hashNub = hashNubOn id
{-# INLINE hashNub #-}

-- | The 'hashNubOn' function behaves just like 'ordNub',
--   except it uses a another type to determine equivalence classes.
--
-- >>> hashNubOn fst [(True, 'x'), (False, 'y'), (True, 'z')]
-- [(True,'x'),(False,'y')]
--
hashNubOn :: (Witherable t, Hashable b) => (a -> b) -> t a -> t a
hashNubOn p t = evalState (witherM f t) HS.empty
  where
    f a = state $ \s ->
      let g Nothing  = BoolPair False (Just ())
          g (Just _) = BoolPair True  (Just ())
      -- there is no HashS.alterF, but toMap / fromMap are newtype wrappers.
      in case HM.alterF g (p a) (HS.toMap s) of
        BoolPair True  s' -> (Nothing, HS.fromMap s')
        BoolPair False s' -> (Just a,  HS.fromMap s')
{-# INLINE hashNubOn #-}


-- used to implement *Nub functions.
data BoolPair a = BoolPair !Bool a deriving Functor
