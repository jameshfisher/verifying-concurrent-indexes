-- Mostly taken from Chris Okasaki,
-- "FUNCTIONAL PEARLS: Red-Black Trees in a Functional Setting".


data Color = Red | Black
             deriving(Show)

data Tree a = Empty
            | Tree Color (Tree a) a (Tree a)
              deriving(Show)

empty :: Tree a
empty = Empty

member :: Ord a => a -> Tree a -> Bool
member x Empty = False
member x (Tree _ left value right)
    = case (compare x value) of
        LT -> member x left
        EQ -> True
        GT -> member x right

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

balance (Tree Black (Tree Red (Tree Red a x                     b) y           c)  z d)   = Tree Red (Tree Black a x b) y (Tree Black c z d)
balance (Tree Black                     a x (Tree Red           b  y (Tree Red c   z d))) = Tree Red (Tree Black a x b) y (Tree Black c z d)
balance (Tree Black (Tree Red           a x (Tree Red           b  y           c)) z d)   = Tree Red (Tree Black a x b) y (Tree Black c z d)
balance (Tree Black                     a x (Tree Red (Tree Red b  y           c)  z d))  = Tree Red (Tree Black a x b) y (Tree Black c z d)
balance t = t