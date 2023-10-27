# Modern Map Merge

This is an attempt at providing more generic, safer merge
functionality for maplike data structures. It has been proposed to the
maintainers of the
  [containers](https://hackage.haskell.org/package/containers)
library as an
  [issue](https://github.com/haskell/containers/issues/937).

There is a
  [blogpost](rationale/blogpost.md)
explaining the rationale for this project. This README is a more
practical account of what this repository contains.

## Packages: serious code

### mini-witherable

This is a slight rewrite of Kinoshita's
  [witherable](https://hackage.haskell.org/package/witherable)
package, written to have a few dependencies as possible (and hence to
make it suitable as a dependency for
  [containers](https://hackage.haskell.org/package/containers),
if desired).

Contents:

* [notes.md](prospective-approach/src-witherable/README.md)

    A more complete account of changes made to Kinoshita's package

* [Data/Filterable.hs](prospective-approach/src-witherable/Data/Filterable.hs)

    Data structures which can have elements removed from them

* [Data/Filterable/WithIndex.hs](prospective-approach/src-witherable/Data/Filterable/WithIndex.hs)

    As above, but indexed

* [Data/Witherable.hs](prospective-approach/src-witherable/Data/Witherable.hs)

    Data structures which can have elements removed, but in a functor

* [Data/Witherable/WithIndex.hs](prospective-approach/src-witherable/Data/Witherable/WithIndex.hs)

    As above, but indexed


### ModernMapMerge

This as the name suggests, is the core of the project: it consists of
the merge functionality of Map and IntMap, rewritten with a shared,
safer interface for merge tactics.

Contents:

* [Data/MergeTactics.hs](prospective-approach/src-modernmapmerge/Data/MergeTactics.hs)

    A collection of merge tactics, replacing those for Map and IntMap

* [Data/MergeTactics/Reindex.hs](prospective-approach/src-modernmapmerge/Data/MergeTactics/Reindex.hs)

    Some code for reindexing merge tactics, probably not of much use to the casual user

* [Data/IMaybe.hs](prospective-approach/src-modernmapmerge/Data/IMaybe.hs)

    An indexed Maybe. Possibly instructive to the user as a simple
    example but also important in applications.

* [Data/IntMap/Merge.hs](prospective-approach/src-modernmapmerge/Data/IntMap/Merge.hs)

    Merge functionality for IntMap

* [Data/Map/Merge.hs](prospective-approach/src-modernmapmerge/Data/Map/Merge.hs)

    Merge functionality for Map


## Packages: proof-of-concept code

### ListMap

This is a toy implementation of an alternative Map data structure,
backed by lists of key-value pairs sorted by order of key.

It is unlikely that anyone will want to use it but it might be a
helpful tutorial for people wishing to implement other maplike data
structures.

Contents:

* [Data/ListMap.hs](prospective-approach/src-listmap/Data/ListMap.hs)

    List-backed maps.

### Maplike

This consists of a typeclass designed to reflect the functionality of
a type like `Map`. It is intended as proof-of-concept rather than a
production implementation: it is not very full-featured.

Also implemented is a generic prefix tree map datatype, where the
nodes can in turn be any Maplike datatype.

Contents:

* [Data/Maplike.hs](prospective-approach/src-maplike/Data/Maplike.hs)

    The Maplike class

* [Data/PrefixMap.hs](prospective-approach/src-maplike/Data/PrefixMap.hs)

    Generic prefix trees

### Extra

This consists largely of the advanced functionality in Kinoshita's
`witherable` package. The plan to make collections depend on
`witherable` requires a version of `witherable` with as few
dependencies as possible; this package contains extra functionality.

It is possible that some or all of this functionality could be moved
into the packages on which it depends.

Contents:

* [Control/Monad/State/Extra.hs](prospective-approach/src-extra/Control/Monad/State/Extra.hs)

    Monad functionality, which could possibly be moved into
      [transformers](https://hackage.haskell.org/package/transformers)

* [Control/Monad/Transformers/Extra.hs](prospective-approach/src-extra/Control/Monad/Transformers/Extra.hs)

    More of the same.

* [Data/Functor/Extra.hs](prospective-approach/src-extra/Data/Functor/Extra.hs)

    Some instances for functor compositions

* [Data/Functor/ExtraUndecidable.hs](prospective-approach/src-extra/Data/Functor/ExtraUndecidable.hs)

    More of the above

* [Data/HashMap/Extra.hs](prospective-approach/src-extra/Data/HashMap/Extra.hs)

    Instances for HashMap and HashSet (which could be moved into
      [unordered-containers](https://hackage.haskell.org/package/unordered-containers))

* [Data/Hashable/Extra.hs](prospective-approach/src-extra/Data/Hashable/Extra.hs)

    Some assorted functionality associated to the above

* [Data/Vector/Extra.hs](prospective-approach/src-extra/Data/Vector/Extra.hs)

    Instances for Vector (which could be moved into
      [vector](https://hackage.haskell.org/package/vector))
