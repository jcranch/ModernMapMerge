-- | Provides support for a spacetree which can have at most one key;
-- an exception is raised if there is any attempt to add a second.
--
-- This is, of course, not normally of much use, but can be useful in
-- compositional `Maplike` instances. For example 
module Data.SpaceTree.Lonely where


data Lonely p v =
  Alone p v |
  Barren
