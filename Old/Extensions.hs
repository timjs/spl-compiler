module Data.Map.Extensions
  ( introduce
  , fromList'
  , (|->)
  , (\/)
  , (/\)
  ) where

import Data.Map (Map, insert, member, empty, union, intersection)
import Data.List (foldl')

infixr 1 |->
--infix n \/ /\

fromList' :: (Ord k) => [(k,v)] -> Map k v
fromList' = foldl' intro empty
  where intro m (k,v) = introduce k v m

introduce :: (Ord k) => k -> v -> Map k v -> Map k v
introduce k v m
  | k `member` m = error $ "keys multiple defined"
  | otherwise    = insert k v m

(|->) :: a -> b -> (a,b)
(|->) = (,)

(\/) :: (Ord k) => Map k v -> Map k v -> Map k v
(\/) = union

(/\) :: (Ord k) => Map k v -> Map k v -> Map k v
(/\) = intersection

extend :: (Ord k) => Map k v -> [(k,v)] -> Map k v
extend = foldl' ins
  where ins m (k,v) = insert k v m

