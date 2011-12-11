{-# LANGUAGE UnicodeSyntax, GADTs #-}

module Marshal (fromList, toList, toSeq, toSet) where

import qualified Data.Set (empty, insert)
import Data.Set (Set)

import qualified Data.Sequence (empty)
import Data.Sequence (Seq, ViewL(..), viewl, (><), singleton)

import Nat (Nat)
import Nodes (B(..), R(..), Tree(..))
import LLRB (Tree(..), insert, empty)

-------

class ToSeq τ where
  toSeq' :: (Nat δ, Ord α)⇒ τ δ α → Seq α

s :: (ToSeq τ, ToSeq τ', Nat δ, Ord α)⇒ τ δ α → α → τ' δ α → Seq α
s l v r = (toSeq' l) >< (singleton v) >< (toSeq' r)

instance ToSeq B where
  toSeq' Nil = Data.Sequence.empty
  toSeq' (B2 l v r) = s l v r
  toSeq' (B3 l v r) = s l v r

instance ToSeq R where
  toSeq' (R  l v r) = s l v r


-- why does this not exist in Data.Sequence?
seq2list :: Data.Sequence.Seq α → [α]
seq2list s = case viewl s of
  EmptyL  → []
  x :< xs → (x:seq2list xs)

toSeq :: Tree α → Seq α
toSeq (Tree t) = toSeq' t

-------

fromList :: Ord α ⇒ [α] → Tree α
fromList = foldl insert empty

toList :: Tree α → [α]
toList = seq2list . toSeq

--------

toSet :: Ord α ⇒ Tree α → Set α
toSet t = foldl (flip Data.Set.insert) Data.Set.empty (toList t)

