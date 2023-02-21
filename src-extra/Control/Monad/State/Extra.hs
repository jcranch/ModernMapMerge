module Control.Monad.State.Extra where

import Control.Monad.Trans.State.Lazy

import Data.Traversable.WithIndex
import Data.Witherable.WithIndex


-- | Traversal with accumulating state
traverseAccum :: (Monad m, Traversable t)
              => (a -> s -> m (b, s))
              -> s
              -> t a
              -> m (t b, s)
traverseAccum f x a = runStateT (mapM (StateT . f) a) x

-- | Indexed traversal with accumulating state
itraverseAccum :: (Monad m, TraversableWithIndex i t)
               => (i -> a -> s -> m (b, s))
               -> s
               -> t a
               -> m (t b, s)
itraverseAccum f x a = runStateT (imapM (\i -> StateT . f i) a) x

-- | Filtering with accumulating state
filterAccum :: (Witherable t)
            => (a -> s -> (Bool, s))
            -> s
            -> t a
            -> (t a, s)
filterAccum f x a = runState (filterM (state . f) a) x

-- | Indexed filtering with accumulating state
ifilterAccum :: (WitherableWithIndex i t)
            => (i -> a -> s -> (Bool, s))
            -> s
            -> t a
            -> (t a, s)
ifilterAccum f x a = runState (ifilterM (\i -> state . f i) a) x

-- | Monadic filtering with accumulating state
filterMAccum :: (Monad m, Witherable t)
            => (a -> s -> m (Bool, s))
            -> s
            -> t a
            -> m (t a, s)
filterMAccum f x a = runStateT (filterM (StateT . f) a) x

-- | Indexed monadic filtering with accumulating state
ifilterMAccum :: (Monad m, WitherableWithIndex i t)
            => (i -> a -> s -> m (Bool, s))
            -> s
            -> t a
            -> m (t a, s)
ifilterMAccum f x a = runStateT (ifilterM (\i -> StateT . f i) a) x

-- | Withering with accumulating state
witherAccum :: (Monad m, Witherable t)
            => (a -> s -> m (Maybe b, s))
            -> s
            -> t a
            -> m (t b, s)
witherAccum f x a = runStateT (witherM (StateT . f) a) x

-- | Indexed withering with accumulating state
iwitherAccum :: (Monad m, WitherableWithIndex i t)
             => (i -> a -> s -> m (Maybe b, s))
             -> s
             -> t a
             -> m (t b, s)
iwitherAccum f x a = runStateT (iwitherM (\i -> StateT . f i) a) x
