{-# LANGUAGE UnicodeSyntax, GADTs, MultiParamTypeClasses #-}

module Insert (Insert(..), insertT) where

import Nat
import Nodes (B (..), R (..), RV(..), Tree(..), blk)


insert' ::
  (Insert μ  ε,   -- a μ might turn into a error tree of type ε upon insertion left; the ε must then be fixed.
   Insert μ' ε',  -- as above for right insertion.
   Nat δ, Nat δ', -- two depths.
   Ord α) ⇒
   (μ δ α → α → μ' δ' α → τ)  -- the data constructor, of type τ, that wrapped the following three parameters:
   → μ δ α    --   left subtree,
   → α        --   root value,
   → μ' δ' α  --   right subtree.
   → α        -- value to insert.
   → (ε δ α → α → μ' δ' α → Either τ τ')  -- leftFix
   → (μ δ α → α → ε' δ' α → Either τ τ')  -- rightFix.  Both return either the original type τ, or a new tree type τ'.
   → Either τ τ'  -- one of the fixers might be applied or the data constructor, giving either a τ or a τ'.
insert' dc l v r x leftFix rightFix = case compare x v of
  EQ → Left $ dc l v r     -- just wrap up the tree again.
  LT → case insert l x of  -- insert left;
    Left  l' → Left $ dc l' v r  -- if it came back as the same type of tree, wrap it up again;
    Right l' → leftFix   l' v r  -- otherwise, apply the leftFix.
  GT → case insert r x of  -- equivalent for inserting right.
    Left  r' → Left $ dc l v r'
    Right r' → rightFix  l v r'

class Insert μ ε where
  insert ∷ (Nat δ, Ord α)⇒ μ δ α → α → Either (μ δ α) (ε δ α)

instance Insert R RV where
  insert (R l v r) x = insert' R l v r x leftFix rightFix where
    leftFix  l' v' r'               = Right $ RV l'            v'  r'
    rightFix l' v' (R rl' rv' rr')  = Right $ RV (R l' v' rl') rv' rr'

instance Insert B R where
  insert Nil x = Right $ R Nil x Nil

  insert (B2 l v r) x = insert' B2 l v r x leftFix rightFix where
    leftFix  l' v' r'               = Left  $ B3 l'            v'  r'
    rightFix l' v' (R rl' rv' rr')  = Left  $ B3 (R l' v' rl') rv' rr'

  insert (B3 l v r) x = insert' B3 l v r x leftFix rightFix where
    leftFix  (RV ll' lv' lr') v' r' = Right $ R  (blk ll')     lv' (B2 lr' v' r')
    rightFix l'               v' r' = Right $ R  (blk l')      v'  (blk r')

insertB2B :: (Nat δ, Ord α)⇒ B δ α → α → Either (B δ α) (B (S δ) α)
insertB2B t v = case insert t v of
  Left  t' → Left  $     t'
  Right t' → Right $ blk t'

-- the end-user function.
insertT :: (Ord α)⇒ Tree α → α → Tree α
insertT (Tree t) v = case insertB2B t v of
    Left  t' → Tree t'
    Right t' → Tree t'