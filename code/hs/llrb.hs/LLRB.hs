{-# LANGUAGE UnicodeSyntax, GADTs #-}

-- End-user module.
module LLRB (Tree, empty, search, insert, remove) where


import Nodes (B(..), Tree(..))
import qualified Search (Search(..))
import qualified Insert (insertT)
import qualified Remove (removeT)


empty :: Ord α ⇒ Tree α
empty = Tree Nil

search :: Ord α ⇒ Tree α → α → Bool
search (Tree t) v = Search.search t v

insert :: Ord α ⇒ Tree α → α → Tree α
insert = Insert.insertT

remove :: Ord α ⇒ Tree α → α → Tree α
remove = Remove.removeT
