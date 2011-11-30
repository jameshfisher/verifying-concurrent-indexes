{-# LANGUAGE OverloadedStrings #-}

module LineTypes (typedLines, TypedLine(CommentLine, AssertionLine, CodeLine)) where

import Prelude hiding (null, length, takeWhile, dropWhile, concat)
import Data.Text      (null, length, takeWhile, dropWhile, concat, Text, uncons)

import Lex (Line(..))

data TypedLine = CommentLine   !Int !Text
               | AssertionLine !Int !Text
               | CodeLine           !Text

typedLines :: [Line] -> [TypedLine]
typedLines = map typedLine

typedLine :: Line -> TypedLine
typedLine (Line indent code comment)
  | null code = case (length comment) of
    0 -> CodeLine ""
    _ -> case slashLength comment of
            2 -> CommentLine (length indent) (stripComment comment)
            _ -> AssertionLine (length indent) (stripComment comment)
  | otherwise = CodeLine (concat [indent, code, comment])

dropLeadingSpace :: Text -> Text
dropLeadingSpace t = case uncons t of
  Nothing       -> t
  Just (hd, tl) -> case hd of
                   ' ' -> tl
                   _   -> t

stripComment :: Text -> Text
stripComment = dropLeadingSpace . dropWhile (\c -> c == '/')

slashLength :: Text -> Int
slashLength = length . takeWhile (\c -> c == '/')
