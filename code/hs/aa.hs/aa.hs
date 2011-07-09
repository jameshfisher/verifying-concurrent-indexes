
data AATree a = Nil | AATree Int (AATree a) a (AATree a)
            deriving (Show, Eq)

aaInsert :: Ord a => a -> AATree a -> AATree a
aaInsert x Nil = AATree 1 Nil x Nil
aaInsert x (AATree level left v right) = case (compare x v) of
                                   EQ -> (AATree level left v right)
                                   LT -> (aaSplit . aaSkew) (AATree level (aaInsert x left) v right             )
                                   GT -> (aaSplit . aaSkew) (AATree level left              v (aaInsert x right))

aaSkew :: AATree a -> AATree a
aaSkew (AATree level (AATree leftLevel leftLeft leftVal leftRight) val right) =
    if level == leftLevel
    then (AATree leftLevel leftLeft leftVal (AATree level leftRight val right))
    else (AATree level (AATree leftLevel leftLeft leftVal leftRight) val right)
aaSkew t = t


aaSplit :: AATree a -> AATree a
aaSplit (AATree level left val (AATree rightLevel rightLeft rightVal (AATree rightRightLevel rightRightLeft rightRightVal rightRightRight))) =
    if level == rightLevel && rightLevel == rightRightLevel
    then (AATree (rightLevel + 1) (AATree level left val rightLeft) rightVal (AATree rightRightLevel rightRightLeft rightRightVal rightRightRight))
    else (AATree level left val (AATree rightLevel rightLeft rightVal (AATree rightRightLevel rightRightLeft rightRightVal rightRightRight)))
aaSplit t = t