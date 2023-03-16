# A Withering Glance: Map merging in Haskell



## The problem

The module
[`Data.Map`](https://hackage.haskell.org/package/containers-0.6.3.1/docs/Data-Map.html)
is a widely-used data structure for storing key-value pairs in great
generality (well, reasonable generality: for nearly all purposes the
type of keys has to have an instance of `Ord`). As such, it is the
rough equivalent of Python's dicts; it is a good persistent,
functional alternative to hash tables for many purposes.

Part of its popularity, no doubt, comes from the versatility with
which they can be combined key-by-key. This is most familiar in the
classic set-theoretic operations of union, intersection, and
difference, but more bespoke operations are frequently useful. For
example, a data structure representing counts of different kinds of
objects might be implemented internally as `Map k Int`, and then
[`unionWith (+)`](https://hackage.haskell.org/package/containers-0.6.3.1/docs/Data-Map-Strict.html#g:12)
is a combination function which adds everything up. For example,
here's how to work out how many pets of various sorts two people have
between them:

    λ> import qualified Data.Map as M
    λ> myPets = M.fromList [("cats", 3), ("dogs", 2)]
    λ> yourPets = M.fromList [("cats", 2), ("hamsters", 1)]
    λ> M.unionWith (+) myPets yourPets
    fromList [("cats",5),("dogs",2),("hamsters",1)]



## Map merging in the past

Up until the mid-2010s, versions of the `containers` library had an
efficient
[merge operation](https://hackage.haskell.org/package/containers-0.5.7.1/docs/Data-Map-Strict.html#g:12)
with a fairly lengthy type:

    mergeWithKey :: Ord k
                 => (k -> a -> b -> Maybe c)
                 -- ^ "combine"
                 -> (Map k a -> Map k c)
                 -- ^ "only1"
                 -> (Map k b -> Map k c)
                 -- ^ "only2"
                 -> Map k a
                 -- ^ input map 1
                 -> Map k b
                 -- ^ input map 2
                 -> Map k c
                 -- ^ output map

The documentation had this to say about the first three inputs:

> When calling `mergeWithKey combine only1 only2`:
>
> * if a key is present in both maps, it is passed with both
>   corresponding values to the `combine` function. Depending on the
>   result, the key is either present in the result with specified
>   value, or is left out;
> * a nonempty subtree present only in the first map is passed to
>   `only1` and the output is added to the result;
> * a nonempty subtree present only in the second map is passed to
>   `only2` and the output is added to the result.

The documentation had this warning about the `only1` and `only2`
tactics (emphasis as in the original):

> The `only1` and `only2` methods... *must return a map with a subset
> (possibly empty) of the keys of the given map*

This restriction is not just there to reflect the semantics, but is a
safety requirement. Since the results of these tactics are inserted as
complete chunks into the corresponding part of the resulting data
structure, a bad choice of these methods (such as `M.mapKeys (+
1000000)`) could result in an invalid map: a binary tree with keys on
the left that are greater than keys on the right.

There are further possibilities for breaking things that are so
perverse that the writers of the library may not have even thought of
them. For example, one could take `only1` to be the function

    \m -> fmap (const (M.size m)) m

which replaces every value of `m` by the number of entries in
`m`. This is bad because the answers depend on the size of the subtree
it's handed, which in turn depends on how the shapes of the trees
happen to match up. In a compliant implementation, every value of the
output should depend only on the key and the value of the input.

Another irritating feature is the lack of genericity: the tactics for
a merge are specific to `Data.Map`, and hence do not work for other
maplike data structures; this problem is discussed more later.



## Map merging in the present

This interface changed some years ago, from version 0.5.8.1 of
`containers`, released in August 2016.

The signature of the function, now
[simply known as `merge`](https://hackage.haskell.org/package/containers-0.6.3.1/docs/Data-Map-Merge-Strict.html),
became

    merge :: Ord k => SimpleWhenMissing k a c
                   -- ^ What to do with keys in m1 but not m2
                   -> SimpleWhenMissing k b c
                   -- ^ What to do with keys in m2 but not m1
                   -> SimpleWhenMatched k a b c
                   -- ^ What to do with keys in both m1 and m2
                   -> Map k a
                   -> Map k b
                   -> Map k c

In fact (besides a small change to the underlying algorithm) this
embodies several changes, which I'll summarise in turn:


### Encapsulation

As can be seen, we now have specially named types for the merge
tactics. In the case of `SimpleWhenMatched`, this mostly just has the
effect of giving types names that explain what they do. The definition
of `SimpleWhenMatched` is isomorphic to the following (but, for
reasons to be explained shortly, implemented in slightly more
generality):

    newtype SimpleWhenMatched k a b c = SimpleWhenMatched
      { matchedKey :: k -> a -> b -> Maybe c }

and that is just a newtype for the previous version's `combine`
argument.


### Specialisation of `WhenMissing`

The representation of `SimpleWhenMissing` tactics, on the other hand,
have been changed from previous versions so as to store two different
ways of handling missing data: one key at a time, and a whole tree at
a time. The definition is isomorphic to (but, again, actually more
general than) this:

    data SimpleWhenMissing k a b = SimpleWhenMissing
      { missingSubtree :: Map k a -> Map k b
      , missingKey :: k -> a -> Maybe b }

This is duplicate information, and provided only for
efficiency. Indeed, in a compliant implementation each method should
be fully determined by the other:

    missingSubtree = M.mapWithKey missingKey
    missingKey k v = M.lookup k . missingSubtree $ M.singleton k v

In order to assist the user and make user code more readable, a number
of smart constructors of merge tactics have been provided, such as
`preserveMissing` and `dropMissing`.


### Genericity

You may have been wondering what's so simple about
`SimpleWhenMissing`. The answer is that merging was made significantly
more general, in the sense that `merge` is just a special case of a
more general function taking values in any applicative functor:

    mergeA :: (Applicative f, Ord k)
           => WhenMissing f k a c
           -- ^ What to do with keys in m1 but not m2
           -> WhenMissing f k b c
           -- ^ What to do with keys in m2 but not m1
           -> WhenMatched f k a b c
           -- ^ What to do with keys in both m1 and m2
           -> Map k a
           -> Map k b
           -> f (Map k c)

If `f` is `Identity` we just get `merge` back, and `SimpleWhenMissing`
is `WhenMissing Identity`, and `SimpleWhenMatched` is `WhenMatched
Identity`, and it's the presence of the `Identity` newtypes which
explains my deliberate omissions above.

This function `mergeA` has a bewildering array of potential uses for
map combining.

You want to take a union (preferring the second map's value in case of
matches), but keep count of the number of matching keys as you go?
That's no problem: you can do it with the applicative functor `(,)
(Sum Int)`.

You want to do the same thing, but actually keep track of the values
that have been lost, thus taking a union and an intersection
simultaneously, but still only performing only one traverse of the
tree structures? Sure you can: you can do it with the
[product applicative functor](https://hackage.haskell.org/package/base-4.15.0.0/docs/GHC-Generics.html#t::-42-:)
`Identity :*: Identity` (or, for fans of code golf, the
[homogeneous pair-forming applicative functor](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Complex.html#t:Complex)
`Complex` for brevity).

You want to list all possible maps obtained by taking a union, using
all possible conventions for preferring one value over the other in
case of a match? No problem, that's just the list functor `[]`:

    λ> import qualified Data.Map as M
    λ> import qualified Data.Map.Merge.Strict as M
    λ> compromises = M.mergeA M.preserveMissing M.preserveMissing (M.zipWithAMatched (\_ r s -> if r == s then [r] else [r,s]))
    λ> m1 = M.fromList [("breakfast","cereal"), ("lunch","soup"), ("dinner","stew"), ("drink","beer")]
    λ> m2 = M.fromList [("lunch","sandwich"), ("dinner","stew"), ("drink","wine")]
    λ> compromises m1 m2
    [fromList [("breakfast","cereal"),("dinner","stew"),("drink","beer"),("lunch","soup")],
     fromList [("breakfast","cereal"),("dinner","stew"),("drink","beer"),("lunch","sandwich")],
     fromList [("breakfast","cereal"),("dinner","stew"),("drink","wine"),("lunch","soup")],
     fromList [("breakfast","cereal"),("dinner","stew"),("drink","wine"),("lunch","sandwich")]]


### Safety

The new implementation does provide some increased safety for the
novice user, since only the standard tactics are imported by default
and these are guaranteed safe to use, while the power to create new
tactics is locked away in an `Internal` library. This
security-through-indirection has become commonplace in Haskell and
it's no bad thing.

However, while the default tactics provided are *theoretically* fully
general, this theoretical generality can only be achieved by using
functions which fall back to the key-by-key behaviour. There are
natural tactics which can't be implemented *efficiently* using the
defaults: an example is the `WhenMissing` tactics, for the example
above of calculating a union and intersection simultaneously, which
are built using a `preserveMissing` on the first coordinate and a
`dropMissing` on the second.


### Specificity

The situation for genericity actually got slightly worse with these
changes. There are many other data structures which store arbitrary
key-value pairs:

* [`IntMap`](https://hackage.haskell.org/package/containers-0.6.5.1/docs/Data-IntMap-Strict.html),
  a more efficient replacement for `Map Int`;
* [`Data.HashMap`](https://hackage.haskell.org/package/unordered-containers-0.2.14.0/docs/Data-HashMap-Strict.html),
  based on hash tries rather than binary search, and which may be more efficient in some circumstances;
* various
  [mutable hash table](https://hackage.haskell.org/package/hashtables)
  and
  [mutable hash trie](https://hackage.haskell.org/package/stm-containers)
  implementations
* [`Data.Trie`](https://hackage.haskell.org/package/bytestring-trie-0.2.5.0/docs/Data-Trie.html), providing an efficient data structure replacing `Map String`;
* Trie maps, as described by
  [Peyton Jones and Graf](https://cs.paperswithcode.com/paper/triemaps-that-match),
  which provide a data structure with Haskell code as keys;
* Other prefix tree implementations, providing a useful data structure
  for `Map [a]`;
* Spacetrees, which provide data structures for spatial data:
  quadtrees replace `Map (Float, Float)` and octrees replace `Map
  (Float, Float, Float)`;
* Various bespoke combinations, like `Map a (Map b v)` as a variant of
  `Map (a,b) v` where it seems convenient, or `(Map a v, Map b v)` as
  a variant of `Map (Either a b) v)` where it seems convenient.

The issue is that all of these can be provided with good tactic-based
merge functions, all of which are (to varying degrees) faster than the
naive ones, but implementing each requires basically the entire
tactics code to be copied. This has been
[done](http://downloads.haskell.org/~ghc/latest/docs/html/libraries/containers-0.6.4.1/Data-IntMap-Merge-Strict.html)
for `IntMap`. If only it could be done once and for all in a way that
works nicely for anything resembling a map! (Spoiler: read on...)



## Map merging in the future?

To summarise the above, the present merge framework has some
downsides:

 * The framework has `Data.Map` baked into it, and any use for any
   other purpose requires essentially the whole of the tactics code to
   be copied in full.
 * While a range of commonplace uses are provided by a safe library,
   the desire for efficiency requires one to define specialised
   tactics (for exotic applicative functors) by hand occasionally.
 * Defining `WhenMissing` tactics by hand involves duplication of
   labour (specifying the same behaviour in two different ways).
 * That duplication also duplicates the possibility for obscure bugs
   (if the two different ways are accidentally mismatched).

Is it possible to provide a framework for map merging which is *fully
generic* (that is, will work for any fairly similar key-value data
structure), *fully safe* (that is, without any possibility of illegal
tactics), *fully succinct* (that is, requiring no duplicate labour),
and *equally efficient* in practice? I believe this is nearly
possible, and here's how.

The main ingredient is the
  [witherable](https://hackage.haskell.org/package/witherable) package,
by Fumiaki Kinoshita, which builds on top of the
  [indexed-traversable](https://hackage.haskell.org/package/indexed-traversable) package, to supply some useful typeclasses for maps.


### Abstracting `merge`

To warm up, I'll explain how to abstract the `merge` operation, even
though the ultimate goal is of course `mergeA`. An instance `m` of the
[`Filterable`](https://www.stackage.org/haddock/nightly-2021-04-02/witherable-0.4.1/Witherable.html#t:Filterable)
typeclass (such as `Data.Map`) has a
[filtering operation](https://www.stackage.org/haddock/nightly-2021-04-02/witherable-0.4.1/Witherable.html#v:mapMaybe):

    mapMaybe : (a -> Maybe b) -> m a -> m b

There is an
["indexed" variant](https://www.stackage.org/haddock/nightly-2021-04-02/witherable-0.4.1/Witherable.html#t:FilterableWithIndex),
`FilterableWithIndex`, which has an indexed filtering operation:

    imapMaybe : (i -> a -> Maybe b) -> m a -> m b

This time, `Map i` is an instance of `FilterableWithIndex i`: the
method `imapMaybe` is
[`M.mapMaybeWithKey`](https://hackage.haskell.org/package/containers-0.5.7.1/docs/Data-Map-Strict.html#v:mapMaybeWithKey).
An `imapMaybe` is *theoretically* precisely the sort of thing you
might do to make a compliant `SimpleWhenMissing` tactic, but is
inefficient in practice.

However, a reasonable way of defining a `SimpleWhenMissing` tactic to
be something that operates on *all* `FilterableWithIndex` instances:

    data Filtering i a b = Filtering {
      runFilter :: forall m. FilterableWithIndex i m => m a -> m b }

What examples are there? Well, we can use any functions that are
available on any `FilterableWithIndex`, which includes any of the
superclasses of `FilterableWithIndex` (including `Filterable`,
[`Foldable`](https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Foldable.html),
and
[`Functor`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Functor.html),
and their
[indexed versions](https://hackage.haskell.org/package/indexed-traversable)).

So we can do

    Filtering (imapMaybe f)

to implement a very general merge tactic (equivalent to
[`mapMaybeMissing`](https://hackage.haskell.org/package/containers-0.6.3.1/docs/Data-Map-Merge-Lazy.html#v:mapMaybeMissing))
in the current library, and

    Filtering (imap f)

(using
[`imap`](https://hackage.haskell.org/package/indexed-traversable-0.1.1/docs/Data-Functor-WithIndex.html#v:imap)
from
[`FunctorWithIndex`](https://hackage.haskell.org/package/indexed-traversable-0.1.1/docs/Data-Functor-WithIndex.html#v:imap))
to get an equivalent of
[`mapMissing`](https://hackage.haskell.org/package/containers-0.6.3.1/docs/Data-Map-Merge-Lazy.html#v:mapMissing),
which doesn't delete any elements and hence runs faster since it does
not rebalance the tree, and

    Filtering id

to do the equivalent of
[`preserveMissing`](https://hackage.haskell.org/package/containers-0.6.3.1/docs/Data-Map-Merge-Lazy.html#v:preserveMissing)
in the standard library; this runs very fast indeed since it really is
just the identity function. The neat thing is that none of these
require pre-prepared tactics: they are just functions that work on a
`FilterableWithIndex` such as `Map`.

This tells us what to do on a map, but what about the equivalent of
`missingKey`? The solution here is to build an instance of
`FilterableWithIndex` that can only store one key:

    newtype IMaybe i x = IMaybe {
      iMaybe :: Maybe (i,x) }

    instance FilterableWithIndex i (IMaybe i) where
      imapMaybe f (IMaybe a) = IMaybe (itraverse f =<< a)

We can recover `missingKey` by inspecting the action of a `Filtering`
on an `IMaybe` (in the reasonable hope that the optimising compiler
strips out the abstraction).

Hence one correct answer might be: one proper type for a
`SimpleWhenMissing` tactic is, in fact, a `Filtering`. But we won't do
that, because abstracting `mergeA` is the goal.


### Abstracting `mergeA`

The problem of abstracting `mergeA` is similar, but requires working
in a different typeclass. Recall that an instance `m` of the
[`Traversable`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Traversable.html#t:Traversable)
typeclass has a method

    traverse :: Applicative f => (a -> f b) -> m a -> f (m b)

The
[`Witherable`](https://www.stackage.org/haddock/nightly-2021-04-02/witherable-0.4.1/Witherable.html#t:Witherable)
typeclass can be thought of as a common generalisation of `Filterable`
and `Traversable`; it has a method

    wither :: Applicative f => (a -> f (Maybe b)) -> m a -> f (m b)

which generalises `filter` if we take `f` to be `Identity`, and
generalises `traverse` in the sense that

    traverse k = wither (fmap Just . k)

The
[`WitherableWithKey`](https://www.stackage.org/haddock/nightly-2021-04-02/witherable-0.4.1/Witherable.html#t:WitherableWithIndex)
typeclass is an indexed variant of that:

    wither :: Applicative f => (i -> a -> f (Maybe b)) -> m a -> f (m b)

Using this typeclass, we can imitate the previous section (and also
build the applicative functor action in):

    newtype WhenMissing f i x y = WhenMissing {
      runWhenMissing :: forall n. WitherableWithIndex i n => n x -> f (n y) }

This is the proper type for a `WhenMissing` tactic, and allows you to
readily define tactics which operate with the right level of
efficiency. The types `Map` and `IMaybe` as defined above also have a
`WitherableWithIndex` implementation, so they can be acted upon by a
`WhenMissing`; this replaces `missingTree` and `missingKey`.


### Dropping subtrees

There's one tactic I have refrained from mentioning amidst the
optimism of the last two sections: the difficulty of efficiently
dropping a subtree (`dropMissing` in the current library). This can be
accommodated, but I feel that to do so nicely will involve a small
amount of library redesign.

The issue is that `mapMaybe (const Nothing)` is a constant function
for most instances of `Filterable`: the output is always the same
empty map object. It is desirable that it be implemented as a constant
function where possible, since a good, efficient `dropMissing` should
quickly return the canonical example of an empty map, rather than
traversing the structure to throw it all away.

This is a rationale for adding a method

    drain :: m a -> m b

to `Filterable` which is a specialisation of `mapMaybe (const
Nothing)`; with this dropMissing could be implemented as `WhenMissing
(pure . drain)`.



## Conclusions

This proposal meets its demands fairly well:

* It is *fully generic*, in the sense that one tactic library for
  maplike data structures will suffice for all of them.
* It is *fully succinct* in the sense that each tactic requires only
  one definition.
* I think it ought to be *equally efficient*, but have not
  investigated. This is making some assumptions of the quality of the
  optimiser, but the Haskell community displays apparently
  well-deserved pride in the ability of GHC to deal with things like
  this correctly.
* It is only *partly safe*, in the sense that some devious operations
  are still possible. For example, one can still set all the values of
  a map to be equal to the size of the map (which can be extracted
  from any `WitherableWithIndex`, or indeed any `Foldable`, by taking
  `foldMap (const (Sum 1))`: that is, adding together 1's for each
  element in the structure). However:
  * It seems significantly less likely that a naive user will do
    something like this than they are to build a tactic out of
    something like `mapKeys (+ 1000000)`.
  * It is possible that linear typing in future versions of Haskell
    will improve the safety further (by insuring that each key/value
    contributes to the result at most once).



## Disclaimers: loose ends


### Strict merge tactics

The current merge code for Data.Map comes in two versions, lazy and
strict. The strict version has a merge tactic
[`preserveMissing'`](https://hackage.haskell.org/package/containers-0.6.5.1/docs/Data-Map-Merge-Strict.html#v:preserveMissing-39-)
which forces the keys but otherwise preserves the map. This can't be
reproduced exactly using the functionality available.

As it is, there are two ways of dealing with this:

* Live with it, and use slower forcing methods instead (likely to be
  unpopular, although it's hard to say how many people routinely
  use this functionality to force keys);
* Add a `force` method somewhere in the hierarchy. There is a danger
  that this won't always be hugely meaningful: in this case it could
  be implemented as an `id` (with an apology to the user in the
  documentation).


### Merges for HashMap

This new merging framework would certainly make it possible to add a
`mergeA` to
[`Data.HashMap`](https://hackage.haskell.org/package/unordered-containers-0.2.19.1/docs/Data-HashMap-Strict.html).
It is, however, a somewhat daunting task, particularly as the
maintainers have very high standards for performance.

If I were to attempt to write one, I'd produce an internal datatype
storing a bitmap/array pair together with a function that realises it
as a `HashMap` (which could be empty, a leaf/collision, or a partial
or full node), provide withering and merging functionality for
bitmap/array pairs, and have the merging for `HashMap` be mutually
recursive with that for bitmap/array pairs.



## Acknowledgements

Thanks are due to [Tom Ellis](https://github.com/tomjaguarpaw) for
helping me develop these ideas.