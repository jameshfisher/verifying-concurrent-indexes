{-# LANGUAGE UnicodeSyntax #-}

module Remove where

import Nat
import Nodes

moveRedLeft :: RT a h → Either (RT a h) (B4T a h)
-- l is B2T
moveRedLeft (RT (B2T ll lv lr) v (B2T rl rv rr)) = B4T (RT ll lv lr) v (RT rl rv rr)
moveRedLeft (RT (B2T ll lv lr) v (B3T (RT rll rlv rlr) rv rr)) = RT (B3T (RT ll lv lr) v rll) rlv (B2T rlr rv rr)



removeB3 :: R a h  →  a  →  B a h  →  a  →  Either (B a (S h)) (B a h)

removeB  l v r  x = case compare x v of
  LT → case removeR l x of
    Left  l' → Left  $ B3 l' v r
    Right l' → Right $ B2 l' v r
  GT → 

removeR :: R a h → a → Either (R a h) (B a h)


