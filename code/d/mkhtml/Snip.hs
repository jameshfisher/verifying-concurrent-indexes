{-# LANGUAGE OverloadedStrings #-}

module Snip (snip) where

import Safe (tailSafe)
import Data.Text (Text, isPrefixOf, isSuffixOf)

import LineTypes

prefix :: Text -> TypedLine -> Bool
prefix pref line = case line of
  (CommentLine _ cmt) -> isPrefixOf pref cmt
  _ -> False

snipStart :: TypedLine -> Bool
snipStart = prefix "[snip:start]"
snipEnd :: TypedLine -> Bool
snipEnd = prefix "[snip:end]"

span' :: (a -> Bool) -> [a] -> ([a], [a])
span' p xs = let (t, f) = span p xs in (t, tailSafe f)

snipIn :: [TypedLine] -> [TypedLine]
snipIn [] = []
snipIn ls = let (keep, rest) = span' (not . snipStart) ls in keep ++ (snipOut rest)

snipOut :: [TypedLine] -> [TypedLine]
snipOut [] = []
snipOut    ls = let (_, rest) = span' (not . snipEnd)   ls in         (snipIn rest)

snipLines :: [TypedLine] -> [TypedLine]
snipLines = filter (not . isSnipLine) where
  isSnipLine (CommentLine _ cmt) =  isSuffixOf "// [snip]" cmt
  isSnipLine _ = False

snip :: [TypedLine] -> [TypedLine]
snip = snipLines . snipIn