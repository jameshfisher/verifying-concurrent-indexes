{-# LANGUAGE UnicodeSyntax, GeneralizedNewtypeDeriving #-}

module Test (prop_fromListToListIsSameElements) where

import Data.Set ((\\), singleton)
import qualified Data.Set (insert, null)
import Data.List (group, sort)
import Test.QuickCheck (quickCheck, printTestCase, Arbitrary(..), choose, Gen)

import Nodes (Tree(..), R(..))
import LLRB (Tree, empty, insert, remove)
import Marshal (fromList, toList, toSet)

instance (Arbitrary a, Ord a)⇒ Arbitrary (Tree a) where
  arbitrary = do
    size ← choose (0, 100) :: Gen Int
    elements ← sequence $ replicate size arbitrary
    return $ fromList elements


newtype TestInt = TestInt Int deriving (Eq, Ord, Num)
instance Show TestInt where
  show (TestInt i) = show i
instance Arbitrary TestInt where
  arbitrary = do
    i ← choose (0, 1000)
    return $ TestInt i
 

--maybeToBool (Just _) = True
--maybeToBool Nothing  = False

-- this should exist
unique :: Ord α ⇒ [α] → [α]
unique = (map head) . group . sort

-- this should exist
ordered :: Ord a ⇒ [a] → Bool
ordered [] = True
ordered [x] = True
ordered (x:y:xs) = x < y && ordered (y:xs)


prop_fromListToListIsSameElements :: [TestInt] → Bool
prop_fromListToListIsSameElements xs = (unique xs) == unique (toList $ fromList xs)

type EmptyTest = TestInt → Bool
type OpTest = Tree TestInt → TestInt → Bool

prop_insertInserts :: OpTest
prop_insertInserts t x = (toSet $ insert t x) == (Data.Set.insert x $ toSet t)

prop_insertMaintainsOrder :: OpTest
prop_insertMaintainsOrder t x = ordered $ toList $ insert t x

prop_insertIntoEmpty :: EmptyTest
prop_insertIntoEmpty x = (toSet $ insert empty x) == (singleton x)

prop_removeRemoves :: OpTest
prop_removeRemoves t x = (toSet $ remove t x) == ((toSet t) \\ (singleton x))

prop_removeMaintainsOrder :: OpTest
prop_removeMaintainsOrder t x = ordered $ toList $ remove t x

prop_removeFromEmpty :: EmptyTest
prop_removeFromEmpty x = Data.Set.null $ toSet $ remove empty x

prop_insertIdempotent :: OpTest
prop_insertIdempotent t x = (toSet $ insert t x) == (toSet $ insert (insert t x) x)

prop_removeIdempotent :: OpTest
prop_removeIdempotent t x = (toSet $ remove t x) == (toSet $ remove (remove t x) x)

-- prop_insertThenRemoveIsSameElements,
-- prop_removeThenInsertIsSameElements -- not true you idiot!

main = do
  quickCheck prop_fromListToListIsSameElements
  quickCheck prop_insertInserts
  quickCheck prop_insertMaintainsOrder
  quickCheck prop_insertIntoEmpty
  quickCheck prop_removeRemoves
  quickCheck prop_removeMaintainsOrder
  quickCheck prop_removeFromEmpty
  quickCheck prop_insertIdempotent
  quickCheck prop_removeIdempotent