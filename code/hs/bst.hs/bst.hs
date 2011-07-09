-- A basic Binary Search Tree.

data Tree a = Empty
           | Tree (Tree a) a (Tree a)
             deriving(Show, Eq)

insert x Empty = Tree Empty x Empty
insert x (Tree left v right) = case (compare x v) of
                                 LT -> Tree (insert x left) v right
                                 EQ -> Tree left v right
                                 GT -> Tree left v (insert x right)

insertList [] t = t
insertList (x:xs) t = insertList xs (insert x t)

treeDelete x Empty = Empty
treeDelete x (Tree left v right) = case (compare x v) of
                                 LT -> Tree (treeDelete x left) v right
                                 GT -> Tree left v (treeDelete x right)
                                 EQ -> deleteThis left right

deleteThis left right
    | left == Empty && right == Empty = Empty
    | left == Empty && right /= Empty = right
    | left /= Empty && right == Empty = left
    | left /= Empty && right /= Empty = Tree (treeDelete z left) z right
    where z = (treeMax left)

treeMax (Tree _ x Empty) = x
treeMax (Tree _ _ right) = (treeMax right)
                                      