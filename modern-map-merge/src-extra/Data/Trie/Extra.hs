module Data.Trie.Extra where

import Data.ByteString
import Data.Foldable.WithIndex
import Data.Functor.WithIndex
import Data.Traversable.WithIndex
import Data.Trie
import qualified Data.Trie.Internal as I
import Witherable


instance FunctorWithIndex ByteString Trie where
  -- This one doesn't already exist
  imap f = let
    go _ Empty = Empty
    go q (Arc k Nothing t) = Arc k Nothing $ go (q +>! k) t
    go q (Arc k (Just v) t) = let
      q' = toStrict (q +>? k)
      in Arc k (Just $ f q' v) (go (fromStrict q') t)
    go q (Branch p m l r) = Branch p m (go q l) (go q r)
    in go Nil

instance FoldableWithIndex ByteString Trie where
  ifoldl = I.foldlWithKey

instance TraversableWithIndex ByteString Trie where
  itraverse = I.traverseWithKey

instance Filterable Trie where
  mapMaybe = I.filterMap

instance FilterableWithIndex ByteString Trie where
  imapMaybe = I.mapBy

instance Witherable Trie where
  wither = I.wither

instance WitherableWithIndex ByteString Trie where

-- TODO Write merge, then mergeA by analogy with
-- https://hackage.haskell.org/package/bytestring-trie-0.2.7.2/docs/src/Data.Trie.Internal.html#mergeBy
-- https://hackage.haskell.org/package/bytestring-trie-0.2.7.2/docs/src/Data.Trie.Internal.html#intersectBy
