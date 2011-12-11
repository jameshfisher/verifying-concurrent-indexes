{-# LANGUAGE UnicodeSyntax, GADTs #-}

module Nodes (B (..), R (..), RV(..), RB(..), Tree(..)) where

import Nat (Nat, S, O)
import Print (Str(..), red, sh)


-- Data constructors
--------------------

-- two type constructors: B and R.
-- (B δ α) is a black-rooted tree at depth δ (Peano-encoded), containing elements of type a.
-- (R δ α) is a red-rooted tree at depth δ.

-- the following fully encodes the LLRB constraints
-- (but not the set the tree contains!).

data B δ α where  -- a black-rooted tree at depth δ can have three forms:
  Nil ∷ (Ord α       )⇒                            B    O  α   -- an empty tree at height zero.
  B2  ∷ (Nat δ, Ord α)⇒  B δ α  →  α  →  B δ α  →  B (S δ) α   -- two black-rooted children, at depth δ-1;
  B3  ∷ (Nat δ, Ord α)⇒  R δ α  →  α  →  B δ α  →  B (S δ) α   -- one red left, one black right, both αt depth δ-1.

-- a red-rooted tree at depth δ only has one form:
-- two black-rooted children at the same height.
data (Nat δ, Ord α)⇒ R δ α = R (B δ α) α (B δ α)

-- Either of the valid tree types.
data (Nat δ, Ord α)⇒ RB δ α = RnotB (R δ α) | BnotR (B δ α)

data (Nat δ, Ord α)⇒ RV δ α = RV (R δ α) α (B δ α)  -- Red Violation: red root, red left child. Requires fixing.


class Blacken τ where
  blacken :: (Nat δ, Ord α)⇒ τ δ α → Either (B δ α) (B (S δ) α)

instance Blacken B where
  blacken b = Left b

instance Blacken R where
  blacken (R l v r) = Right $ B2 l v r

instance Blacken RB where
  blacken (RnotB t) = blacken t
  blacken (BnotR t) = blacken t

instance Blacken RV where
  blacken (RV l v r) = Right $ B3 l v r


-- The end-user data type, void of heights.
data Tree α where
  Tree ∷ (Nat δ, Ord α)⇒ B δ α → Tree α

instance (Show α, Ord α)⇒ Show (Tree α) where
  show (Tree t) = str t 0 "─→"


class Blacken τ ⇒ ToTree τ where
  toTree :: (Nat δ, Ord α)⇒ τ δ α → Tree α
  toTree t = case blacken t of
    Left t'  → Tree t'
    Right t' → Tree t'


-- Show instances
-----------------

s ∷ (Show a, Show b, Show c) ⇒ String → a → b → c → String

s n l v r = "(" ++ n ++ " " ++ (show l) ++ " " ++ (show v) ++ " " ++ (show r) ++ ")"

instance (Show α, Nat δ, Ord α)⇒ Str (B δ α) where
  str Nil _ _ = ""
  str (B2 l v r) i pre = sh l r i (pre ++ show v)
  str (B3 l v r) i pre = sh l r i (pre ++ show v)

instance (Show α, Nat δ, Ord α)⇒ Str (R δ α) where
  str (R  l v r) i pre = sh l r i (pre ++ (red $ show v))


instance (Show α, Nat δ, Ord α)⇒ Show (B δ α) where
  show Nil = "Nil"
  show (B2 l v r) = s "B2" l v r
  show (B3 l v r) = s "B3" l v r

instance (Show α, Nat δ, Ord α)⇒ Show (R δ α) where
  show (R l v r) = s "R" l v r

instance (Show α, Nat δ, Ord α)⇒ Show (RB δ α) where
  show (RnotB t) = show t
  show (BnotR t) = show t

-- do the rest