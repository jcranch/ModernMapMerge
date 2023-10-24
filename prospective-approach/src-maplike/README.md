## Maplike

This consists of a typeclass designed to reflect the functionality of
a type like `Map`. It is intended as proof-of-concept rather than a
production implementation: it is not very full-featured.

Also implemented is a generic prefix tree map datatype, where the
nodes can in turn be any Maplike datatype.

Contents:

* [Data/Maplike.hs](src-maplike/Data/Maplike.hs)

    The Maplike class

* [Data/PrefixMap.hs](src-maplike/Data/PrefixMap.hs)

    Generic prefix trees
