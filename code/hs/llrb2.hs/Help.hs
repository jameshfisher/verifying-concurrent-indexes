{-# LANGUAGE UnicodeSyntax #-}

module Help (maybeToBool, unique, ordered) where

import Data.List (group, sort)

maybeToBool (Just _) = True
maybeToBool Nothing  = False

-- this should exist
unique :: Ord α ⇒ [α] → [α]
unique = (map head) . group . sort

-- this should exist
ordered :: Ord a ⇒ [a] → Bool
ordered [] = True
ordered [x] = True
ordered (x:y:xs) = x < y && ordered (y:xs)
