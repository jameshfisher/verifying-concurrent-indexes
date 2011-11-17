{-# LANGUAGE UnicodeSyntax, GADTs, MultiParamTypeClasses #-}

module Insert where

import Nat
import Nodes

-- Violation: red root, red left child. Requires fixing one level up.
data (Ord a, Nat h)⇒ RV a h = RV (R a h) a (B a h)

blk (R l v r) = B2 l v r -- increases height by one.

insert' dc l v r x leftFix rightFix = case compare x v of
  EQ → Left $ dc l v r
  LT → case insert l x of
    Left  l' → Left $ dc l' v r
    Right l' → leftFix  l' v r
  GT → case insert r x of
    Left  r' → Left $ dc l v r'
    Right r' → rightFix l v r'

class Insert n m where
  insert ∷ (Ord a, Nat h)⇒ n a h → a → Either (n a h) (m a h)

instance Insert R RV where
  insert (R l v r) x = insert' R l v r x leftFix rightFix where
    leftFix	 l' v r            = Right $ RV l'         v  r   --
    rightFix l  v (R rl rv rr) = Right $ RV (R l v rl) rv rr  --

instance Insert B R where
  insert Nil x = Right $ R Nil x Nil

  insert (B2 l v r) x = insert' B2 l v r x leftFix rightFix where
    leftFix  l' v r             = Left  $ B3 l'          v  r --
    rightFix r  v (R rl rv rr)  = Left  $ B3 (R l v rl) rv rr --

  insert (B3 l v r) x = insert' B3 l v r x leftFix rightFix where
    leftFix  (RV ll lv lr) v r  = Right $ R (blk ll) lv (B2 lr v r)
    rightFix l             v r' = Right $ R (blk l)  v  (blk r') --

  insert (B4 l v r) x = insert' R (blk l) v (blk r) x leftFix rightFix where
    leftFix l' = 


insRB ∷ (Ord a) ⇒ RB a → a → RB a
insRB (RB t) a = case (insert t a) of
  Left  t' → RB       t'
  Right t' → RB $ blk t'
