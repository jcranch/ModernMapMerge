Changes from the original
=========================

This is a trimmed-down version of Fumiaki Kinoshita's Witherable
library. What's been changed?

* Fewer dependencies

  Any functions or instances dependent upon packages other than
  (base)[https://hackage.haskell.org/package/base-4.16.0.0/docs/index.html],
  (containers)[https://hackage.haskell.org/package/containers-0.6.5.1], and
  (indexed-traversable)[https://hackage.haskell.org/package/indexed-traversable]
  have been removed to provide a version with minimal dependencies.

* Flush, saviour of the universe

  The flush method has been added to Filterable, which is a
  (frequently significantly more efficient) synonym for

        mapMaybe (const Nothing)

* Instances for Kleisli

  I added an instance of `FunctorWithIndex` to `Kleisli m a` (from
  `Control.Arrow` in base) to the indexed-traversable
  package. Correspondingly, there are instances of `Filterable` and
  `FilterableWithIndex` in this package.

* Mild restructuring

  The format has been altered to coincide more closely with the base
  libraries and indexed-traversable: Filterable and Witherable, and
  their indexed versions, have all been split into separate
  modules. The indexed versions export the functionality of the
  nonindexed versions.

* No non-compliant instances

  The original package provided instances of FilterableWithIndex and
  WitherableWithIndex for lists, sequences and vectors which are not
  compliant with the advertised axioms; the problem being that indices
  are assigned consecutively from zero, and thus filtering will change
  the indices, which is not permitted. For example, an instance of
  FilterableWithIndex is supposed to have

        ifilter f . ifilter g ≡ ifilter (\i -> liftA2 (&&) (f i) (g i))

  but if we have

        f i _ = even i
        g i _ = even i

  then we have

        (ifilter f . ifilter g) [0..10]                 🠒 [0,4,8]
		ifilter (\i -> liftA2 (&&) (f i) (g i)) [0..10] 🠒 [0,2,4,6,8,10]

  since the first removes the odd indices, and then removes the odd
  indices in the result, while the second simply removes the odd
  indices.

  These functions do of course provide useful functionality, but
  should not be available in this form.

* No undecidable instances

  The undecidable instances (for the Sum, Compose and Product
  functors) have been removed. I suspect it would be possible to
  strengthen GHC's algorithm for resolving functional dependencies to
  make these all decidable.

* Where's it all gone?

  See the "extras" library for much of the missing functionality and
  more besides (decidable versions of the undecidable instances above,
  for example).

  The eventual form of the "extras" library will depend upon how much
  of this ends up in the containers library.
