{-# LANGUAGE EmptyDataDecls, GADTs, KindSignatures #-}

module Nodes (O, S, BT (..), RT (..)) where

-- Peano naturals at type level.
-- 0 = O
-- 1 = S O
-- 2 = S (S O)
-- ...
data O
data S n

-- two type constructors: BT and RT.
-- (BT a h) is a black-rooted tree at height h (Peano-encoded), containing elements of type a.
-- (RT a h) is a red-rooted tree at height h.

-- the following fully encodes the LLRB constraints
-- (but no specification of the set the tree contains).

data BT a h where  -- a black-rooted tree at height h can have four forms:
  Nil :: Ord a => BT a O                                     -- an empty tree at height zero.
  B2T :: Ord a => BT a h_1 -> a -> BT a h_1 -> BT a (S h_1)  -- two black-rooted children, at height h-1;
  B3T :: Ord a => RT a h_1 -> a -> BT a h_1 -> BT a (S h_1)  -- one red left, one black right, both at height h-1;
  B4T :: Ord a => RT a h_1 -> a -> RT a h_1 -> BT a (S h_1)  -- two red-rooted at h-1.

data RT a h where  -- a red-rooted tree at height h only has one form:
  RT :: Ord a => BT a h -> a -> BT a h -> RT a h  -- two black-rooted children at the same height.


s n l v r = n ++ " (" ++ (show l) ++ " " ++ (show v) ++ " " ++ (show r) ++ ")"

instance Show a => Show (BT a h) where
  show Nil = "Nil"
  show (B2T l v r) = s "B2T" l v r
  show (B3T l v r) = s "B3T" l v r
  show (B4T l v r) = s "B3T" l v r

instance Show a => Show (RT a h) where
  show (RT l v r) = s "RT" l v r
