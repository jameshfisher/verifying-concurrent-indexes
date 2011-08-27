type Weight = Int

data CT = Router Weight CT Int CT
        | Leaf Weight Int
        | Empty
        deriving (Show)

insert (Router w l v r) x = case x > v of
  False -> insert l x
  True -> insert r x

insert l@(Leaf w v) x = case compare x v of
  EQ -> l
  LT -> Router (w-1) (Leaf 1 x) v (Leaf 1 v)
  GT -> Router (w-1) (Leaf 1 v) v (Leaf 1 x)

insert Empty x = Leaf 1 x

blacken i (Router w l v r) = Router i l v r
blacken i (Leaf w v) = Leaf i v

remove n@(Router w l v r) x = case x > v of
  False -> case l of
    (Router _ _ _ _) -> remove l x
    (Leaf lw lv) -> case x == v of
      False -> n
      True -> blacken (w+lw) r
  True -> case r of
    (Router _ _ _ _) -> remove r x
    (Leaf rw rv) -> blacken (w+rw) l
    
remove l@(Leaf w v) x = case x == v of
  True -> Empty
  False -> l




data C = C Weight C Int C
       | Empty
       deriving (Show)

