module Search where

import Nodes

class Search a where
  search :: a -> Int -> Bool

instance Search BT where
  search  ET               x = False
  search (FBT v       l r) x = s v l r x
  search (EBT v       l r) x = s v l r x
  search (LBT v False l r) x = s v l r x
  search (LBT v True  r l) x = s v l r x

instance Search RT where
  search (RT  v l r) x = s v l r x

s :: (Search a, Search b) => Int -> a -> b -> Int -> Bool
s v l r x = case compare x v of
  LT -> search l x
  EQ -> True
  GT -> search r x
