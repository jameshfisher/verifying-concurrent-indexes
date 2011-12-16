{-# LANGUAGE UnicodeSyntax, GADTs #-}

module Nodes (Nil(..), B2(..), B3(..), B (..), R (..), RV(..), RB(..), Tree(..), nil, b2, b3, mkB, blk) where

import Nat (Nat, S, O)
import Print (Str(..), red, sh)


-- Data constructors
--------------------

-- a black-rooted tree at depth δ can have three forms:
data Nil δ α where Nil ∷ (       Ord α)⇒                            Nil   O  α   -- an empty tree at height zero.
data B2  δ α where  B2 ∷ (Nat δ, Ord α)⇒  B δ α  →  α  →  B δ α  →  B2 (S δ) α   -- two black-rooted children, at depth δ-1;
data B3  δ α where  B3 ∷ (Nat δ, Ord α)⇒  R δ α  →  α  →  B δ α  →  B3 (S δ) α   -- one red left, one black right, both αt depth δ-1.

data (Nat δ, Ord α)⇒ B δ α = Nil' (Nil δ α) | B2' (B2 δ α) | B3' (B3 δ α)

-- a red-rooted tree at depth δ only has one form: two black-rooted children at the same height.
data (Nat δ, Ord α)⇒ R δ α = R (B δ α) α (B δ α)

-- Either of the valid tree types.
data (Nat δ, Ord α)⇒ RB δ α = R' (R δ α) | B' (B δ α)

-- Red Violation: red root, red left child. Requires fixing.
data (Nat δ, Ord α)⇒ RV δ α = RV (R δ α) α (B δ α)

nil :: (Ord α)⇒ B O α
nil = Nil' Nil

b2 :: (Nat δ, Ord α)⇒  B δ α  →  α  →  B δ α  →  B (S δ) α
b2 l v r = B2' $ B2 l v r

b3 :: (Nat δ, Ord α)⇒  R δ α  →  α  →  B δ α  →  B (S δ) α
b3 l v r = B3' $ B3 l v r

class Blacken τ where
  blacken :: (Nat δ, Ord α)⇒ τ δ α → Either (B (S δ) α) (B δ α)

instance Blacken Nil where blacken = Right . Nil'
instance Blacken B2  where blacken = Right . B2'
instance Blacken B3  where blacken = Right . B3'
instance Blacken B   where
  blacken (Nil' t) = blacken t
  blacken (B2'  t) = blacken t
  blacken (B3'  t) = blacken t
instance Blacken R   where blacken (R l v r) = Left $ B2' $ B2 l v r
instance Blacken RB  where
  blacken (R' t) = blacken t
  blacken (B' t) = blacken t

instance Blacken RV where blacken (RV l v r) = Left $ B3' $ B3 l v r


-- The end-user data type, void of heights.
data Tree α where Tree ∷ (Nat δ, Ord α)⇒ B δ α → Tree α

--instance (Show α, Ord α)⇒ Show (Tree α) where
--  show (Tree t) = str t 0 "─→"


class Blacken τ ⇒ ToTree τ where
  toTree :: (Nat δ, Ord α)⇒ τ δ α → Tree α
  toTree t = case blacken t of
    Left  t' → Tree t'
    Right t' → Tree t'


-- Show instances
-----------------

s ∷ (Show a, Show b, Show c) ⇒ String → a → b → c → String

s n l v r = concat ["(", n, " ", show l, " ", show v, " ", show r, ")"]

instance (Show α, Nat δ, Ord α)⇒ Str (Nil δ α) where str Nil _ _ = ""
instance (Show α, Nat δ, Ord α)⇒ Str (B2  δ α) where str (B2 l v r) i pre = sh l r i (pre ++ show v)
instance (Show α, Nat δ, Ord α)⇒ Str (B3  δ α) where str (B3 l v r) i pre = sh l r i (pre ++ show v)
instance (Show α, Nat δ, Ord α)⇒ Str (R   δ α) where str (R  l v r) i pre = sh l r i (pre ++ (red $ show v))
instance (Show α, Nat δ, Ord α)⇒ Str (B   δ α) where
  str (Nil' t) = str t
  str (B2'  t) = str t
  str (B3'  t) = str t
  

instance (Show α, Nat δ, Ord α)⇒ Show (Nil δ α) where show Nil = "Nil"
instance (Show α, Nat δ, Ord α)⇒ Show (B2  δ α) where show (B2 l v r) = s "B2" l v r
instance (Show α, Nat δ, Ord α)⇒ Show (B3  δ α) where show (B3 l v r) = s "B3" l v r
instance (Show α, Nat δ, Ord α)⇒ Show (B   δ α) where
  show (Nil' t) = show t
  show (B2'  t) = show t
  show (B3'  t) = show t

instance (Show α, Nat δ, Ord α)⇒ Show (R δ α) where
  show (R l v r) = s "R" l v r

instance (Show α, Nat δ, Ord α)⇒ Show (RB δ α) where
  show (R' t) = show t
  show (B' t) = show t


mkB :: (Nat δ, Ord α)⇒ RB δ α → α → B δ α → B (S δ) α
mkB l v r = case l of
  R' l' → B3' $ B3 l' v r
  B' l' → B2' $ B2 l' v r

blk :: (Nat δ, Ord α)⇒ R δ α -> B2 (S δ) α
blk (R l v r) = B2 l v r -- increases height by one.
