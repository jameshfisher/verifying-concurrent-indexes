-- Mostly taken from Chris Okasaki,
-- "FUNCTIONAL PEARLS: Red-Black Trees in a Functional Setting".


data Color = Red | Black
             deriving(Show, Eq)

data Tree a = Empty
            | Tree Color (Tree a) a (Tree a)
              deriving(Show)

empty :: Tree a
empty = Empty

member :: Ord a => a -> Tree a -> Bool
member x Empty = False
member x (Tree _ left value right) = case x == value of
  True -> True
  False -> member x (sub left right (value < x))

sub l _ False = l  
sub _ r True  = r

makeBlack (Tree _     left value right) =
           Tree Black left value right

ins :: Ord a => a -> Tree a -> Tree a
ins x Empty = Tree Red Empty x Empty
ins x (Tree color left value right) =
    case (compare x value) of
      LT -> Tree color (ins x left) value right
      EQ -> Tree color left         value right
      GT -> Tree color left         value (ins x right)

insert :: Ord a => a -> Tree a -> Tree a
insert x t = makeBlack (balance (ins x t))

balance :: Tree a -> Tree a
balance Empty = Empty

balance (Tree Black (Tree Red (Tree Red lll llv                     llr) lv           lr)   v   r  )   = Tree Red (Tree Black lll llv llr) lv (Tree Black lr  v   r  )
balance (Tree Black                     l   v   (Tree Red           rl   rv (Tree Red rrl   rrv rrr))) = Tree Red (Tree Black l   v   rl ) rv (Tree Black rrl rrv rrr)
balance (Tree Black (Tree Red           a x (Tree Red           b  y           c)) z d)   = Tree Red (Tree Black a x b) y (Tree Black c z d)
balance (Tree Black                     a x (Tree Red (Tree Red b  y           c)  z d))  = Tree Red (Tree Black a x b) y (Tree Black c z d)
balance t = t


-- remove value from tree; produce tree with value removed.
remove :: Ord a => a -> Tree a -> Tree a

-- to remove from the whole tree, we go into the recursive call, and blacken the root when we come up.
remove v t = blacken $ newt
          where (newt, _) = remove_aux v t

blacken (Tree _ l v r) = Tree Black l v r
blacken Empty = Empty

-- if Bool is True, the returned tree is at the same black height;
-- if Bool is False, at one less than original BH
remove_aux :: Ord a => a -> Tree a -> (Tree a, Bool)

remove_aux _ Empty          = (Empty, True)  -- removing anything from the empty tree produces the empty tree.
remove_aux x (Tree c l v r) = case compare x v of
  LT -> bal (Tree c newl v r) False y
    where (newl, y) = remove_aux x l
  GT -> bal (Tree c l v newr) True y
    where (newr, y) = remove_aux x r
  EQ -> case l of
    Empty -> tryb r c
    _     -> case r of
      Empty -> tryb l c
      _     -> bal (Tree c newl max r) False y
        where max = findMax l
              (newl, y) = remove_aux max l

tryb t                Red   = (t,                  True )
tryb Empty            Black = (Empty,              False)
tryb (Tree Red l v r) Black = ((Tree Black l v r), True )
tryb t                Black = (t,                  False)


-- bal takes a tree,
-- a bool indicating the side that has a reduced BH,
-- a bool indicating whether or not it is done already,
-- and produces a balanced tree if possible
bal :: Ord a => Tree a -> Bool -> Bool -> (Tree a, Bool)


bal t _ True = (t, True)  -- if it's already balanced, there's nothing to do

-- don't check  for Empty; bal is not be passed Empty

-- balance on the LHS
bal (Tree c l v (Tree rc rl rv rr)) False False = case rc of
  Red -> ((Tree Black newl rv rr), True)                                                       -- passed red sibling; do case reducing rotation
    where (newl, _) = (bal (Tree Red l v rl) False False)                                        -- will always return balanced
  Black -> case rr of                                                                          -- passed black sibling.
    Tree Red rrl rrv rrr -> ((Tree c (Tree Black l v rl) rv (Tree Black rrl rrv rrr)), True)     -- red far child, do single rotate, done
    _ -> case rl of                                                                              -- rr is black, maybe Empty. now test rl.
      Tree Red rll rlv rlr -> ((Tree c (Tree Black l v rll) rlv (Tree Black rlr rv rr)), True)     -- red near child, do double rotation :-(, done
      _ -> case c of                                                                               -- r is black, rl is black, rr is black, we can do color swap.
        Red ->   ((Tree Black l v (Tree Red rl rv rr)), True )                                       -- root is Red, we can color it black and stop.
        Black -> ((Tree Black l v (Tree Red rl rv rr)), False)                                       -- root is Black, even up the right child and pass the violation up.

bal (Tree _ _ _ Empty) _ _ = error "bal must not be passed non-null right sibling"

-- balance on the RHS
bal (Tree c (Tree lc ll lv lr) v r) True False = case lc of -- [ll < lv <       lr < v < r]
  Red -> ((Tree Black ll lv newr), True) -- case reduce.       [ll < lv < newr@[lr < v < r]]
    where (newr, _) = (bal (Tree Red lr v r) False False) -- always returns balanced
  Black -> case ll of -- passed black sibling, test far child
    Tree Red lll llv llr -> ((Tree c (Tree Black lll llv llr) lv (Tree Black lr v r)), True) -- far red child, single rotate, done. [ll@[lll < llv < llr] < lv < lr                   < v < r]
    _ -> case lr of -- ll is black, maybe Empty.  test lr.
      Tree Red lrl lrv lrr -> ((Tree c (Tree Black ll lv lrl) lrv (Tree Black lrr v r)), True) -- red near child, double  >:-@.     [ll                   < lv < lr@[lrl < lrv < lrr] < v < r]
      _ -> case c of -- lr is black, as is ll, as is l, we can color it red
        Red   -> ((Tree Black (Tree Red ll lv lr) v r), True ) -- Stop here!                                                        [ll < lv < lr < v < r]
        Black -> ((Tree Black (Tree Red ll lv lr) v r), False) -- pass up.                                                          [ll < lv < lr < v < r]


bal Empty _ _ = error "bal should be passed non-null tree"

findMax (Tree _ _ v Empty) = v
findMax (Tree _ _ _ r) = findMax r


rbinfo_aux Empty _ _ = Just (Black, 0)
rbinfo_aux (Tree c l v r) lo hi = case (cmp lo v hi) of
  False -> Nothing
  True -> rbok (rbinfo_aux l lo (Just v)) (rbinfo_aux r (Just v) hi) c
    where
      rbok Nothing       _           _ = Nothing
      rbok _             Nothing     _ = Nothing
      rbok (Just (lc, lh)) (Just (rc, rh)) c = case lh == rh of
        False -> Nothing
        True -> case c of
          Black -> Just (Black, lh+1)
          Red -> case (lc == Black && rc == Black) of
            False -> Nothing
            True -> Just (Red, lh)
  where    
    cmp Nothing v Nothing = True
    cmp Nothing v (Just hi) = v < hi
    cmp (Just lo) v Nothing = lo < v
    cmp (Just lo) v (Just hi) = lo < v && v < hi      
                                                                                                            

rbinfo t = rbinfo_aux t Nothing Nothing


--prop_against_model [] = True
--prop_against_model (op:ops) t= case op of
--  Ins x -> 

main = do return ()