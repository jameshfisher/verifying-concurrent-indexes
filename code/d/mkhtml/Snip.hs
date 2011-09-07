module Snip (snip, startsWith, endsWith) where

import LineTypes

startsWith :: Eq a => [a] -> [a] -> Bool
startsWith [] _ = True
startsWith (w:ws) (s:ss) = (w == s) && startsWith ws ss
startsWith _ _ = False

endsWith :: Eq a => [a] -> [a] -> Bool
endsWith w s = startsWith (reverse w) (reverse s)

snipBlocks :: [TypedLine] -> [TypedLine]
snipBlocks ((CommentLine _ "[snip:start]"):ls) = notEnd ls
     where notEnd ((CommentLine _ "[snip:end]"):ls) = ls
           notEnd (l:ls) = notEnd ls
           notEnd [] = []
snipBlocks (l:ls) = l:(snip ls)
snipBlocks [] = []

dropIf f = filter (not . f)

snipLines = dropIf snipLine where
          snipLine (CodeLine s) = endsWith "// [snip]" s
          snipLine _ = False

snip = snipLines . snipBlocks