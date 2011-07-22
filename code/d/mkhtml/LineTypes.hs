module LineTypes (typedLines, TypedLine(CommentLine, AssertionLine, CodeLine)) where

import Lex

data TypedLine = CommentLine Int String | AssertionLine Int String | CodeLine String

typedLines = map typedLine

typedLine :: Line -> TypedLine
typedLine (Line indent "" comment) = case (length comment) of
  0 -> CodeLine ""
  _ -> case slashLength comment of
          2 -> CommentLine (length indent) (stripComment comment)
          _ -> AssertionLine (length indent) (stripComment comment)

typedLine (Line indent code comment) = CodeLine (indent ++ code ++ comment)


stripComment = tail . dropWhile (\c -> c == '/')

slashLength = length . takeWhile (\c -> c == '/')
