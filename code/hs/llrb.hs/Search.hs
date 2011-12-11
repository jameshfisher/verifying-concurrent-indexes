{-# LANGUAGE UnicodeSyntax, GADTs #-}

module Search where

import Nat (Nat)
import Nodes (B(..), R(..))

class Search τ where
  search :: (Nat δ, Ord α)⇒ τ δ α → α → Bool

instance Search B where
  search Nil         _ = False
  search (B2 l v r) x = s l v r x
  search (B3 l v r) x = s l v r x

instance Search R where
  search (R  l v r)  x = s l v r x

s :: (Search τ, Search τ', Nat δ, Ord α) ⇒ τ δ α → α → τ' δ α → α → Bool
s l v r x = case compare x v of
  LT → search l x
  EQ → True
  GT → search r x