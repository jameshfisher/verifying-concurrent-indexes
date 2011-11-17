{-# LANGUAGE UnicodeSyntax, GADTs #-}

import Nat

data Choice3 t1 t2 t3    = C1of3 t1 | C2of3 t2 | C3of3 t3
data Choice4 t1 t2 t3 t4 = C1of4 t1 | C2of4 t2 | C3of4 t3 | C4of4 t4

data Nil = Nil

data B2 a h where
  B2  ∷ (Ord a, Nat h)⇒  B a h  →  a  →  B a h  →  B a (S h)

data B3 a h where
  B3  ∷ (Ord a, Nat h)⇒  R a h  →  a  →  B a h  →  B a (S h)

data B4 a h where
  B4  ∷ (Ord a, Nat h)⇒  R a h  →  a  →  R a h  →  B a (S h)

data R a h where  -- a red-rooted tree at height h only has one form:
  R   ∷ (Ord a, Nat h)⇒  B a h  →  a  →  B a h  →  R a h  -- two black-rooted children at the same height.

type B234 a h = Choice4 Nil (B2 a h) (B3 a h) (B4 a h)
type B23  a h = Choice3 Nil (B2 a h) (B3 a h)
