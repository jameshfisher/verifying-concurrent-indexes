{-# LANGUAGE UnicodeSyntax, GADTs #-}

module Search (searchT) where

import Nat (Nat)
import Nodes (B(..), R(..), RB(..),Tree(..))

class Search τ where
  search :: (Nat δ, Ord α)⇒ τ δ α → α → Maybe α

instance Search B where
  search Nil         _ = Nothing
  search (B2 l v r) x = s l v r x
  search (B3 l v r) x = s l v r x

instance Search R where
  search (R  l v r)  x = s l v r x

instance Search RB where
  search (RnotB t) = search t
  search (BnotR t) = search t

s :: (Search τ, Search τ', Nat δ, Ord α) ⇒ τ δ α → α → τ' δ α → α → Maybe α
s l v r x = case compare x v of
  LT → search l x
  EQ → Just v
  GT → search r x

searchT :: Tree α → α → Maybe α
searchT (Tree t) = search t