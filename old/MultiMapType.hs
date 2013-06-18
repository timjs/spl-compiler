{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Data.MultiMap
  ( MultiMap
  , singleton
  , insert
  , enter
  , union
  , unions
  , Map.empty
  , Map.toList
  ) where

import Data.Monoid

import Data.Map (Map)
import Data.Set (Set)

import qualified Data.Map as Map
import qualified Data.Set as Set

type MultiMap k a = Map k (Set a)
                    --deriving (Show, Eq, Ord)

singleton :: (Ord k, Ord a) => k -> a -> MultiMap k a
singleton k a = Map.singleton k (Set.singleton a)

insert :: (Ord k, Ord a) => k -> a -> MultiMap k a -> MultiMap k a
insert k a m
  | k `Map.member` m = Map.adjust (Set.insert a) k m
  | otherwise        = Map.insert k (Set.singleton a) m

enter :: (Ord k, Ord a) => k -> Set a -> MultiMap k a -> MultiMap k a
enter k s m
  | k `Map.member` m = Map.adjust (Set.union s) k m
  | otherwise        = Map.insert k s m

union :: (Ord k, Ord a) => MultiMap k a -> MultiMap k a -> MultiMap k a
l `union` r
  | Map.null l      = r
  | Map.null r      = l
  | Map.size r == 1 = enter k s l
  | otherwise       = error "union not yet implemented for big MultiMaps"
    where (k,s)     = Map.elemAt 0 r

unions = error "unions not yet implemented for MultiMap"

instance (Ord k, Ord a) => Monoid (MultiMap k a) where
  mempty  = Map.empty
  mappend = union
  mconcat = unions

