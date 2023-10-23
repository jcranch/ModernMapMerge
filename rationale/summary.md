# Executive Summary


## The problem

Currently, the merge tactics used by
  [Data.Map.Merge.Strict](https://hackage.haskell.org/package/containers-0.6.7/docs/Data-Map-Merge-Strict.html)
and
  [Data.IntMap.Merge.Strict](https://hackage.haskell.org/package/containers-0.6.7/docs/Data-IntMap-Merge-Strict.html)
are very similar, but are specialised: code can't be shared between
them, and can't be shared with any other data structure that stores
key/value pairs.

To write `WhenMissing` tactics directly (as is sometimes required),
one must write two separate routines (one for a whole tree, and one
for a single key/value pair). This duplication introduces a
possibility of error.


## A solution

What should a merge tactic be?

The answer is that a `WhenMissing` tactic is something that should be
able to act on *any* instance of some typeclass. But which typeclass?
It must be an extension of several well-known ones, in particular:

 * `Functor`, because we can do `mapWhenMissing` to functorially
   replace the values, and we can map over a functor;

 * `Traversable`, because we can do `traverseMissing` to systematically
   replace values in any `Applicative` functor.

There are further, more highly structured typeclasses defined in the packages
  [indexed-traversable](https://hackage.haskell.org/package/indexed-traversable)
and
  [witherable](https://hackage.haskell.org/package/witherable).

Indeed, the `WitherableWithIndex` typeclass (in the latter package)
provides a function called `iwither`, whose use is similar to
`traverseMaybeMissing` (described in the merge documentation as "the
most powerful `WhenMissing` tactic"). The relevant parts of the
definitions are as follows:

    class WitherableWithIndex i t | t -> i where
        iwither :: Applicative f => (i -> a -> f (Maybe b)) -> t a -> f (t b)

    traverseMaybeMissing :: Applicative f => (i -> a -> f (Maybe b)) -> WhenMissing f i a b

In other words, a `WitherableWithIndex` is something that can have a
`traverseMaybeMissing` done upon it.

Moreover, `WitherableWithIndex` contains all the other classes
(`Functor`, `Traversable`, and others besides) and that means that the
standard `WhenMissing` tactics can be defined in terms of more
specialised functions than `iwither`.
