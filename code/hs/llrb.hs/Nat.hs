module Nat (O, S, Nat) where

-- Peano naturals at type level.
--   0 <=> O
--   1 <=> S O
--   2 <=> S (S O)
-- ... <=> ...
data O
data S n

class Nat n
instance Nat O
instance Nat n => Nat (S n)
