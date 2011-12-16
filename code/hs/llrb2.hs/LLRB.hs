{-# LANGUAGE UnicodeSyntax, GADTs #-}

-- End-user module.
module LLRB (Tree, empty, search, member, insert, remove) where

import Help (maybeToBool)
import Nodes (B(..), Tree(..))
import qualified Search (searchT)
import qualified Insert (insertT)
import qualified Remove (removeT)


empty :: Ord α ⇒ Tree α
empty = Tree Nil

search :: Ord α ⇒ Tree α → α → Maybe α
search = Search.searchT

member :: Ord α ⇒ Tree α → α → Bool
member t x = maybeToBool $ search t x

insert :: Ord α ⇒ Tree α → α → Tree α
insert = Insert.insertT

remove :: Ord α ⇒ Tree α → α → Tree α
remove = Remove.removeT
