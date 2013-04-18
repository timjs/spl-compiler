module Data.MultiMap where

import Data.Monoid

import Data.Map (Map)
import Data.Set (Set)

import qualified Data.Map as Map
import qualified Data.Set as Set

newtype MultiMap k a = MultiMap (Map k (Set a))
                     deriving (Show, Eq, Ord)

empty :: MultiMap k a
empty = MultiMap Map.empty

toList :: (Ord k, Ord a) => MultiMap k a -> [(k,Set a)]
toList (MultiMap m) = Map.toList m

singleton :: (Ord k, Ord a) => k -> a -> MultiMap k a
singleton k a = MultiMap $ Map.singleton k (Set.singleton a)

insert :: (Ord k, Ord a) => k -> a -> MultiMap k a -> MultiMap k a
insert k a (MultiMap m)
  | k `Map.member` m = MultiMap $ Map.adjust (Set.insert a) k m
  | otherwise        = MultiMap $ Map.insert k (Set.singleton a) m

enter :: (Ord k, Ord a) => k -> Set a -> MultiMap k a -> MultiMap k a
enter k s (MultiMap m)
  | k `Map.member` m = MultiMap $ Map.adjust (Set.union s) k m
  | otherwise        = MultiMap $ Map.insert k s m

union :: (Ord k, Ord a) => MultiMap k a -> MultiMap k a -> MultiMap k a
(MultiMap l) `union` (MultiMap r)
  | Map.null l      = MultiMap r
  | Map.null r      = MultiMap l
  | Map.size r == 1 = enter k s (MultiMap l)
  | otherwise       = error "union not yet implemented for big MultiMaps"
    where (k,s)     = Map.elemAt 0 r

unions = error "unions not yet implemented for MultiMap"

instance (Ord k, Ord a) => Monoid (MultiMap k a) where
  mempty  = empty
  mappend = union
  mconcat = unions

