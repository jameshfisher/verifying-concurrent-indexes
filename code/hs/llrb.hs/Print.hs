{-# LANGUAGE GADTs #-}

module Print where

import Nodes

class (Show a) => Str a where
  str :: a -> Int -> String -> String

join = foldl (++) ""
indent n = join (replicate n "  ")

ansi c = ['\27'] ++ "[" ++ c ++ "m"

sh l r i pre = (str r (i+1) "╭→") ++ (indent i) ++ pre ++ "\n" ++ (str l (i+1) "╰→")

instance (Show a, Ord a, Nat h) => Str (BT a h) where
  str Nil i pre = ""
  str (B2T l v r) i pre = sh l r i (pre ++ show v)
  str (B3T l v r) i pre = sh l r i (pre ++ show v)

instance (Show a, Ord a, Nat h) => Str (RT a h) where
  str (RT  l v r) i pre = sh l r i (pre ++ (ansi "31") ++ (show v) ++ (ansi "0"))

instance  (Show a, Ord a) => Show (RBT a) where
  show (RBT t) = str t 0 "─→"



s n l v r = n ++ " (" ++ (show l) ++ " " ++ (show v) ++ " " ++ (show r) ++ ")"

instance (Show a, Nat h) => Show (BT a h) where
  show Nil = "Nil"
  show (B2T l v r) = s "B2T" l v r
  show (B3T l v r) = s "B3T" l v r

instance (Show a, Nat h) => Show (RT a h) where
  show (RT l v r) = s "RT" l v r
