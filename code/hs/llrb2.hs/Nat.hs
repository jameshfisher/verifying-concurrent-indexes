{-# LANGUAGE UnicodeSyntax #-}

module Nat (O, S, Nat) where

-- Peano naturals at type level.
data O
data S ν

class Nat ν
instance Nat O
instance Nat ν ⇒ Nat (S ν)
