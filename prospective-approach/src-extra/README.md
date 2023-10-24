## Extra

This consists largely of the advanced functionality in Kinoshita's
`witherable` package. The plan to make collections depend on
`witherable` requires a version of `witherable` with as few
dependencies as possible; this package contains extra functionality.

It is possible that some or all of this functionality could be moved
into the packages on which it depends.

Contents:

* [Control/Monad/State/Extra.hs](src-extra/Control/Monad/State/Extra.hs)

    Monad functionality, which could possibly be moved into
      [transformers](https://hackage.haskell.org/package/transformers)

* [Control/Monad/Transformers/Extra.hs](src-extra/Control/Monad/Transformers/Extra.hs)

    More of the same.

* [Data/Functor/Extra.hs](src-extra/Data/Functor/Extra.hs)

    Some instances for functor compositions

* [Data/Functor/ExtraUndecidable.hs](src-extra/Data/Functor/ExtraUndecidable.hs)

    More of the above

* [Data/HashMap/Extra.hs](src-extra/Data/HashMap/Extra.hs)

    Instances for HashMap and HashSet (which could be moved into
      [unordered-containers](https://hackage.haskell.org/package/unordered-containers))

* [Data/Hashable/Extra.hs](src-extra/Data/Hashable/Extra.hs)

    Some assorted functionality associated to the above

* [Data/Vector/Extra.hs](src-extra/Data/Vector/Extra.hs)

    Instances for Vector (which could be moved into
      [vector](https://hackage.haskell.org/package/vector))
