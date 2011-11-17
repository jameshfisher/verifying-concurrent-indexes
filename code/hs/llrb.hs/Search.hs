{-# LANGUAGE GADTs #-}

module Search where

import Nodes

class Search t where
  search :: t a h -> a -> Bool

instance Search BT where
  search Nil         _ = False
  search (B2T l v r) x = s l v r x
  search (B3T l v r) x = s l v r x

instance Search RT where
  search (RT l v r)  x = s l v r x

s :: (Search l, Ord a, Search r) => (l a h) -> a -> (r a h) -> a -> Bool
s l v r x = case compare x v of
  LT -> search l x
  EQ -> True
  GT -> search r x
