{-# LANGUAGE UnicodeSyntax #-}

module Rotate (rleftB3, rleftRV, rrightR, elim4) where

import Nat
import Nodes (B2(..), B3(..), B(..), R(..), RV(..), b2, blk)

rleftB3 :: (Nat δ, Ord α) ⇒ B  δ α → α → R δ α → B3 (S δ) α
rleftRV :: (Nat δ, Ord α) ⇒ B  δ α → α → R δ α → RV    δ  α
rrightR :: (Nat δ, Ord α) ⇒ RV δ α → α → B δ α → R  (S δ) α
elim4   :: (Nat δ, Ord α) ⇒ R  δ α → α → R δ α → R  (S δ) α

rleftB3 l v (R rl rv rr) = B3 (R l v rl) rv rr
rleftRV l v (R rl rv rr) = RV (R l v rl) rv rr
rrightR (RV ll lv lr) v r = R (B2' $ blk ll) lv (b2 lr v r)
elim4 l v r = R (B2' $ blk l) v (B2' $ blk r)