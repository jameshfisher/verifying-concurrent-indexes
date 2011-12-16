{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Prelude ((.), ($), (==), (&&), (||), not, (+), (*), (<), Bool(..), Int, compare, Ordering(..), Maybe(..), error, Ord, Eq, Show(..), replicate, return, foldl, (++), succ)
import Data.Sequence (Seq, singleton, (><), empty, viewr, ViewR(..), viewl, ViewL(..))
import Test.QuickCheck (Arbitrary(..), choose, Gen, Property, (==>))
import Control.Monad (sequence)

data Color = Red | Black deriving (Show, Eq)

data (Ord a)⇒ Tree a = Nil | Tree Color (Tree a) a (Tree a)


instance (Ord a, Show a)⇒ Show (Tree a) where
  show = show' 0 "─→ "
    where
      show' n pre (Tree c l v r) = (show' m larr r) ++ "\n" ++ (indent n) ++ pre ++ (color c $ show v) ++ "\n" ++ (show' m rarr l)
        where m = case c of { Red → n; Black → (succ n) }
              larr = case c of { Red → " ╭ "; Black → "╭→ " }
              rarr = case c of { Red → " ╰ "; Black → "╰→ " }
      show' n pre Nil = (indent n) ++ pre ++ "Nil"
      indent n = replicate (n*3) ' '
      color Red s = ansi_red ++ s ++ ansi_reset
      color Black s = s
      ansi c = ['\27'] ++ "[" ++ c ++ "m"
      ansi_red = ansi "31"
      ansi_reset = ansi "0"


color Nil = Black
color (Tree c _ _ _) = c

isRed t = (color t) == Red

search Nil for = False
search (Tree _ l v r) for = case compare for v of
  EQ → True
  LT → search l for
  GT → search r for

-- should these only be on black roots?
rotateLeft (Tree c l v (Tree Red rl rv rr)) =
            Tree c (Tree Red l v rl) rv rr
rotateLeft t = error "rotateLeft must take a non-empty tree with a non-empty red right"

rotateRight (Tree c (Tree Red ll lv lr) v r) =
             Tree c ll lv (Tree Red lr v r)
rotateRight t = error "rotateRight must take a non-empty tree with a non-empty red left"

colorFlip (Tree c1        (Tree c2        ll lv lr) v (Tree c3        rl rv rr)) =
          (Tree (flip c1) (Tree (flip c2) ll lv lr) v (Tree (flip c3) rl rv rr))
  where
    flip Red = Black
    flip Black = Red

insert :: (Ord a)⇒ Tree a → a → Tree a
insert Nil v = Tree Red Nil v Nil
insert t@(Tree _ (Tree Red _ _ _) v (Tree Red _ _ _)) w = insert (colorFlip t) w
insert t@(Tree c l v r) w = case compare w v of
  EQ → t
  LT → bal $ Tree c (insert l w) v r
  GT → bal $ Tree c l v (insert r w)
  where
    bal = tryRotateRight . tryRotateLeft

    tryRotateLeft t@(Tree _ _ _ (Tree Red _ _ _)) = rotateLeft t
    tryRotateLeft t = t

    tryRotateRight t@(Tree _ (Tree Red (Tree Red _ _ _) _ _) _ _) = rotateRight t
    tryRotateRight t = t

blacken :: (Ord a)⇒ Tree a → Tree a
blacken (Tree Red l v r) = Tree Black l v r
blacken t = t

insertRoot t w = blacken $ insert t w


-- Deletion

fixUp :: (Ord a)⇒ Tree a → Tree a
fixUp = tryColorFlip . tryRotateRight . tryRotateLeft
  where
    tryRotateLeft t@(Tree _ _ _ (Tree Red _ _ _)) = rotateLeft t
    tryRotateLeft t = t

    tryRotateRight t@(Tree _ (Tree Red (Tree Red _ _ _) _ _) _ _) = rotateRight t
    tryRotateRight t = t

    tryColorFlip t@(Tree Black (Tree Red _ _ _) v (Tree Red _ _ _)) = colorFlip t
    tryColorFlip t = t

moveRedRight :: (Ord a)⇒ Tree a → Tree a
moveRedRight t@(Tree c l v (Tree Black (Tree Black rll rlv rlr) rv rr)) = fixRedRed $ colorFlip t
  where
    fixRedRed t@(Tree _ (Tree _ (Tree Red _ _ _) _ _) _ _) = colorFlip $ rotateRight t
    fixRedRed t = t
moveRedRight t = error "moveRedRight requires black right and black right-left"

tryMoveRedRight t@(Tree c l v (Tree Black (Tree Black rll rlv rlr) rv rr)) = fixRedRed $ colorFlip t
  where
    fixRedRed t@(Tree _ (Tree _ (Tree Red _ _ _) _ _) _ _) = colorFlip $ rotateRight t
    fixRedRed t = t
tryMoveRedRight t = t

deleteMaxRoot :: (Ord a)⇒ Tree a → Tree a
deleteMaxRoot = blacken . deleteMax . introduceRed

introduceRed :: (Ord a)⇒ Tree a → Tree a
introduceRed Nil = Nil
-- UGH
introduceRed (Tree Red _ _ _) = error "red root in introduceRed"
introduceRed (Tree Black (Tree Black ll lv lr) v (Tree Black rl rv rr)) =
             (Tree Red   (Tree Black ll lv lr) v (Tree Black rl rv rr))
introduceRed t@(Tree Black (Tree Red ll lv lr) v (Tree Black rl rv rr)) = rotateRight t
introduceRed t = t


deleteMax :: (Ord a)⇒ Tree a → Tree a
deleteMax = deleteMax' . tryRotateLeft
  where
    tryRotateLeft t@(Tree _ (Tree Red _ _ _) _ _) = rotateRight t
    tryRotateLeft t = t

    deleteMax' (Tree _ _ _ Nil) = Nil
    deleteMax' t = case tryMoveRedRight t of
      (Tree c l v r) → fixUp $ Tree c l v (deleteMax r) -- Sedgwick says delete LEFT?!
      Nil            → Nil


type Height = Int


data (Ord a)⇒ TreeType a = ValidTree Color Height (Seq a) deriving (Show)

-- is there an idiomatic way to do this?
continue = Just ()
halt = Nothing

ensure True = continue
ensure False = halt

treeType :: (Ord a)⇒ Tree a → Maybe (TreeType a)
treeType Nil = Just $ ValidTree Black 0 empty
treeType (Tree c l v r) = do
  (ValidTree lc lh lseq) ← treeType l
  (ValidTree rc rh rseq) ← treeType r
  ensure $ lh == rh
  ensure $ (c == Black && not (lc == Black && rc == Red)) || (c == Red && lc == Black && rc == Black)
  case (viewr lseq) of { EmptyR → continue; (rest :> biggestleft) → ensure $ biggestleft < v }
  case (viewl rseq) of { EmptyL → continue; (smallestright :< rest) → ensure $ v < smallestright }
  Just $ ValidTree c (if c == Red then lh else (succ lh)) (lseq >< (singleton v) >< rseq)

-- testing
instance (Arbitrary a, Ord a)⇒ Arbitrary (Tree a) where
  arbitrary = do
    size ← choose (0, 10) :: Gen Int
    elements ← sequence $ replicate size arbitrary
    return $ foldl insertRoot Nil elements

newtype TestInt = TestInt Int deriving (Eq, Ord)
instance Show TestInt where
  show (TestInt i) = show i

instance Arbitrary TestInt where
  arbitrary = do
    i ← choose (0, 50)
    return $ TestInt i
 

maybeToBool (Just _) = True
maybeToBool Nothing  = False

prop_validArbitrary t = maybeToBool $ treeType t

prop_deleteMaxRoot :: Tree TestInt → Bool
prop_deleteMaxRoot t = maybeToBool $ treeType $ deleteMaxRoot t
