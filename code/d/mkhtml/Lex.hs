{-# LANGUAGE OverloadedStrings #-}

module Lex (Line(..), tokens) where

-- not really a lexer, just splits into ws, code, and comments

import Prelude hiding (span, lines)
import Data.Text (Text, span, lines, breakOn)

data Line = Line {
  lineIndent  :: !Text,
  lineCode    :: !Text,
  lineComment :: !Text
}

splitLine :: Text -> Line
splitLine t = Line indent code comment
  where (indent, codeAndComment) = span ws t
        (code, comment) = breakOn "//" codeAndComment

        ws c = c == ' '

tokens :: Text -> [Line]
tokens = map splitLine . lines
