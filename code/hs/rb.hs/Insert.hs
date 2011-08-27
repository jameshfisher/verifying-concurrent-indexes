module Insert where

import Nodes


data RVT = RVT Int Bool RT BT -- False left-leaning; True right-leaning


insertB :: BT -> Int -> (Either BT RT)
insertR :: RT -> Int -> (Either RT RVT)

insertB    ET         x = Right (RT x ET ET) -- insert into empty tree yields RT

insertB t@(FBT v l r) x = case compare x v of
  EQ -> Left t                                             -- no insertion required

  LT -> case (insertR l x) of
    Left  newl -> Left  (FBT v  newl           r)          -- returned RT, OK, no rotation needed
    Right newl -> Right (RT  v (blacken newl) (blacken r)) -- returned RVT, color swap, move red up

  GT -> case (insertR r x) of
    Left  newr -> Left  (FBT v  l              newr)
    Right newr -> Right (RT  v (blacken l)    (blacken newr))


-- no rotations or color flips! Bliss.
insertB t@(EBT v l r) x = case compare x v of
  EQ -> Left t

  LT -> case (insertB l x) of
    Left  newl -> Left (EBT v       newl r)
    Right newl -> Left (LBT v False newl r)

  GT -> case (insertB r x) of
    Left  newr -> Left (EBT v       l    newr)
    Right newr -> Left (LBT v True  newr l)


-- insert into left-leaning two-node
insertB t@(LBT v False l r) x = case compare x v of
  EQ -> Left t

  LT -> case (insertR l x) of                   -- insert into RT
    Left  newl -> Left (LBT v False newl r)     -- got back RT, OK
    Right newl -> case newl of                  -- got back RVT, do single or double rotation
      RVT i False ll lr             -> Left (FBT i ll            (RT v lr  r)) -- left-leaning, do single rotation
      RVT i True  (RT j lrl lrr) ll -> Left (FBT j (RT i ll lrl) (RT v lrr r)) -- right-leaning, do double rotation. Eugh.

  GT -> case (insertB r x) of
    Left  newr -> Left (LBT v False l newr)  -- got back BT, still an LBT
    Right newr -> Left (FBT v       l newr)  -- got back RT, now a FBT


-- insert into right-leaning two-node. Symmetrical.
insertB t@(LBT v True  r l) x = case compare x v of
  EQ -> Left t

  LT -> case (insertB l x) of
    Left  newl -> Left (LBT v True  r    newl)  -- got back BT, still LBT
    Right newl -> Left (FBT v       newl r   )  -- got back RT, now FBT

  GT -> case (insertR r x) of
    Left  newr -> Left (LBT v True  newr l   )  -- got back RT, OK
    Right newr -> case newr of -- got back RVT, do single/double
      RVT i True  rr             rl -> Left (FBT i (RT v l rl)   rr)           -- right-leaning, do single rotation
      RVT i False (RT j rll rlr) rr -> Left (FBT j (RT v l rll) (RT i rlr rr)) -- double


insertR t@(RT v l r) x = case compare x v of
  EQ -> Left t

  LT -> case (insertB l x) of
    Left  newl -> Left  (RT  v       newl r)
    Right newl -> Right (RVT v False newl r)

  GT -> case (insertB r x) of
    Left  newr -> Left  (RT  v       l    newr)
    Right newr -> Right (RVT v True  newr l   )


class Blacken a where
  blacken :: a -> BT

instance Blacken RVT where
  blacken (RVT v dir l r) = LBT v dir l r

instance Blacken BT where
  blacken a = a

instance Blacken RT where
  blacken (RT v l r) = EBT v l r


insert :: BT -> Int -> BT
insert t i = case insertB t i of
  Left o -> blacken o
  Right o -> blacken o
