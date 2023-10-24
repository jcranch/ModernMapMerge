## ModernMapMerge

This is part of a proposal for rewriting the merge functionality of
Map and IntMap with a shared, safer interface for merge tactics.

Contents:

* [Data/MergeTactics.hs](src-modernmapmerge/Data/MergeTactics.hs)

    A collection of merge tactics, replacing those for Map and IntMap

* [Data/MergeTactics/Reindex.hs](src-modernmapmerge/Data/MergeTactics/Reindex.hs)

    Some code for reindexing merge tactics, probably not of much use to the casual user

* [Data/IMaybe.hs](src-modernmapmerge/Data/IMaybe.hs)

    An indexed Maybe. Possibly instructive to the user as a simple
    example but also important in applications.

* [Data/IntMap/Merge.hs](src-modernmapmerge/Data/IntMap/Merge.hs)

    Merge functionality for IntMap

* [Data/Map/Merge.hs](src-modernmapmerge/Data/Map/Merge.hs)

    Merge functionality for Map
