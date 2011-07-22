-- not really a lexer, just splits into ws, code, and comments

module Lex where

data Line = Line {
  indent :: String,
  code :: String,
  comment :: String
}

ws c = c == ' '

splitLine s = Line ind code comment
  where (ind, codeAndComment) = span ws s
        (code, comment) = splitComment codeAndComment

splitComment "" = ("", "")
splitComment l@('/':'/':cs) = ("", l)
splitComment (c:cs) = (c:code, comment)
  where (code, comment) = splitComment cs

tokens = map splitLine . lines
