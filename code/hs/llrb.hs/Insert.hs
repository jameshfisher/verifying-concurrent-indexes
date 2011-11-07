{-# LANGUAGE GADTs #-}

module Insert where

import Nodes

insertB :: BT a h -> a -> Either (BT a h) (RT a h)
insertB Nil x = Right $ RT Nil x Nil

insertR :: RT a h -> a -> Either (RT a h) (RVT a h)
insertR (RT l v r) x = case compare x v of
  LT -> case (insertB l x) of
    Left  l' -> RT l' v r
    Right (RT ll lv lr) -> RVT 
  EQ -> Left $ l v r
  GT -> case (insertB r x) of
    Left  l' -> RT l' v r
    Right (RT rl rv rr) -> rotate


blackenRT :: RT a n -> BT a (S n)
blackenRT (RT l v r) = B2T l v r

--flip :: 
--flip (B4T l v r) = RT (blackenRT l) v (blackenRT r)

-- this shouldn't be a data type; never in function return types
-- tree with right-leaning red at root; otherwise valid; trivially fixed
data RLB3T a h where
  RLB3T :: Ord a => BT a h_1 -> a -> RT a h_1 -> RLB3T a (S h_1)

rotateLeft :: RLB3T a n -> BT a n
rotateLeft (RLB3T l v (RT rl rv rr)) = B3T (RT l v rl) rv rr


-- violation: red root, red left child.
-- requires fixing one level up.
data RVT a h where
  RVT :: Ord a => RT a h -> a -> BT a h -> RVT a h

blackenRVT :: RVT a h -> BT a (S h)
blackenRVT (RVT l v r) = B3T l v r
