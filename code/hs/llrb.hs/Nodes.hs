{-# LANGUAGE UnicodeSyntax, GADTs #-}

module Nodes (B (..), R (..), RB (..)) where

import Nat


-- two type constructors: B and R.
-- (B a h) is a black-rooted tree at height h (Peano-encoded), containing elements of type a.
-- (R a h) is a red-rooted tree at height h.

-- the following fully encodes the LLRB constraints
-- (but no specification of the set the tree contains).

data B a h where  -- a black-rooted tree at height h can have three forms:
  Nil ∷ (Ord a       )⇒                            B a O      -- an empty tree at height zero.
  B2  ∷ (Ord a, Nat h)⇒  B a h  →  a  →  B a h  →  B a (S h)  -- two black-rooted children, at height h-1;
  B3  ∷ (Ord a, Nat h)⇒  R a h  →  a  →  B a h  →  B a (S h)  -- one red left, one black right, both at height h-1.
  B4  ∷ (Ord a, Nat h)⇒  R a h  →  a  →  R a h  →  B a (S h)

data R a h where  -- a red-rooted tree at height h only has one form:
  R   ∷ (Ord a, Nat h)⇒  B a h  →  a  →  B a h  →  R a h  -- two black-rooted children at the same height.

data RB a where -- (exists h. B a h)
  RB  ∷ (Ord a, Nat h  )⇒ B a h → RB a






class (Show a)⇒ Str a where
  str ∷ a → Int → String → String

indent = join . indentN where
  indentN = (flip replicate) "  "
  join = foldl (++) ""


ansi_wrap c s = (ansi c) ++ s ++ ansi_reset where
  ansi c = '\27':'[':(c++"m")
  ansi_reset = ansi "0"

red = ansi_wrap "31"

sh l r i pre = (str r (i+1) "╭→") ++ (indent i) ++ pre ++ "\n" ++ (str l (i+1) "╰→")

instance (Show a, Ord a, Nat h)⇒ Str (B a h) where
  str Nil i pre = ""
  str (B2 l v r) i pre = sh l r i (pre ++ show v)
  str (B3 l v r) i pre = sh l r i (pre ++ show v)

instance (Show a, Ord a, Nat h)⇒ Str (R a h) where
  str (R  l v r) i pre = sh l r i (pre ++ (red $ show v))

instance  (Show a, Ord a)⇒ Show (RB a) where
  show (RB t) = str t 0 "─→"



s n l v r = n ++ " (" ++ (show l) ++ " " ++ (show v) ++ " " ++ (show r) ++ ")"

instance (Show a, Nat h)⇒ Show (B a h) where
  show Nil = "Nil"
  show (B2 l v r) = s "B2" l v r
  show (B3 l v r) = s "B3" l v r

instance (Show a, Nat h)⇒ Show (R a h) where
  show (R l v r) = s "R" l v r
