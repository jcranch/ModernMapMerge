## Mini-Witherable

This is a trimmed-down version of Fumiaki Kinoshita's
  [Witherable](https://hackage.haskell.org/package/witherable)
library.


### What's it do?

It provides four classes. These are:

* [Data.Filterable](src/Data/Filterable.hs)

  This is like a functor, but provides the ability to remove elements as well.

        filter even [1,1,2,3,5,8,13]
          🠒 [2,8]

        filter even (M.fromList [("one",1),("two",2),("three",3),("four",4)])
          🠒 M.fromList [("two",2), ("four",4)]

        mapMaybe (\n -> if even n then Just (n+1) else Nothing) [1,1,2,3,5,8,13]
          🠒 [3,9]

* [Data.Witherable](src/Data/Witherable.hs)

  This is a cross between a traverse and a filter: you can remove
  elements, but all this is valued in an applicative functor.

        wither (\n -> if even n then [Just n, Nothing]) [1,1,2,3,5,8,13]
          🠒 [[2,8],[2],[8],[]]

* [Data.Filterable.WithIndex](src/Data/Filterable/WithIndex.hs)

  This is as Filterable, but where there is an immutable index (such
  as a map key) floating around.

        ifilter (\k n -> length k == n) (M.fromList [("one",1),("two",2),("three",3),("four",4)])
	      🠒 M.fromList [("four",4)]

* [Data.Witherable.WithIndex](src/Data/Witherable/WithIndex.hs)

  This is as Witherable, but where there is an immutable index (such
  as a map key) floating around.

Each is in a separate module, but each exports the functionality of
the modules on which it depends. Hence all are exported by
  [Data.Witherable.WithIndex](src/Data/Witherable/WithIndex.hs).


### Why this smaller version?

My long-term aim, documented at
  [Modern Map Merge](https://github.com/jcranch/ModernMapMerge),
is to replace the current map merging interface in
  [Data.Map.Merge.Strict](https://hackage.haskell.org/package/containers-0.7/docs/Data-Map-Merge-Strict.html)
and
  [Data.IntMap.Merge.Strict](https://hackage.haskell.org/package/containers-0.7/docs/Data-IntMap-Merge-Strict.html)
with a single, safer interface usable for any key-value data
structure.

I have submitted a number of pull requests to harmonise the original
Witherable library with this one so far as is possible, but there are
two differences of philosophy which have made this fork inevitable:

* In order to be used in the containers library, all this code plus much of
    [indexed-traversable](https://hackage.haskell.org/package/indexed-traversable)
  must be upstream of containers (in containers itself, in base, or in
  a new module imported by containers). To minimise transitive
  dependencies and increase confidence, this module must be as small
  and as self-contained as possible.

* In order to maximise efficiency, we wish to fuse operations together
  as much as possible using rewrite rules. So that these rewrite rules
  will be correct, we have to insist that instances satisfy strong
  laws.


### What's actually different, compared to Witherable?

I have submitted pull requests to try to harmonise these libraries as
much as possible. But what's been changed compared to the original?

* No non-compliant instances

  The original package provided instances of FilterableWithIndex and
  WitherableWithIndex for lists, sequences and vectors which are not
  compliant with the advertised axioms; the problem being that indices
  are assigned consecutively from zero, and thus filtering will change
  the indices, which is not permitted. For example, an instance of
  FilterableWithIndex is supposed to have

        ifilter f . ifilter g ≡ ifilter (\i x -> f i x && g i x)

  but if we have

        f i _ = even i
        g i _ = even i

  then we have

        (ifilter f . ifilter g) [0..10]          🠒 [0,4,8]
		ifilter (\i x -> f i x && g i x) [0..10] 🠒 [0,2,4,6,8,10]

  since the first removes the odd indices, and then removes the odd
  indices in the result, while the second simply removes the odd
  indices.

  We do however provide functions which supply this functionality
  generically, but do not bundle them into an instance to avoid
  claiming laws which do not hold. Happily, there were no non-lawful
  instances which used a nondefault implementation of iwither.

* Fewer dependencies

  Any functions or instances dependent upon packages other than
  [base](https://hackage.haskell.org/package/base-4.16.0.0/docs/index.html),
  [containers](https://hackage.haskell.org/package/containers-0.6.5.1),
  and
  [indexed-traversable](https://hackage.haskell.org/package/indexed-traversable)
  have been removed to provide a version with minimal dependencies.

* Rewrite rules

  We supply rewrite rules to ensure that chaining together various
  functions is rapid.

* Flush, saviour of the universe

  The flush method has been added to Filterable, which is a
  (frequently significantly more efficient) synonym for

        mapMaybe (const Nothing)

  A similar proposal had been
  [made some years ago](https://github.com/fumieval/witherable/issues/57).

* Instances for Kleisli

  I added an instance of `FunctorWithIndex` to `Kleisli m a` (from
  [Control.Arrow](https://hackage.haskell.org/package/base-4.18.0.0/docs/Control-Arrow.html)
  in base) to the
  [indexed-traversable](https://hackage.haskell.org/package/indexed-traversable)
  package. Correspondingly, there are instances of `Filterable` and
  `FilterableWithIndex` in this package.

* Mild restructuring

  The format has been altered to coincide more closely with the base
  libraries and
  [indexed-traversable](https://hackage.haskell.org/package/indexed-traversable):
  Filterable and Witherable, and their indexed versions, have all been
  split into separate modules. The indexed versions export the
  functionality of the nonindexed versions.

* No undecidable instances

  The undecidable instances (for the Sum, Compose and Product
  functors) have been removed.

* Format compatible with indexed-traversable

  We have split the one module into four, so the

* Where's the rest of the code gone?

  See the "extras" library for much of the missing functionality and
  more besides (decidable versions of the undecidable instances above,
  for example).

  The eventual form of the "extras" library will depend upon how much
  of this ends up in the containers library.
