{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}

module Transform (transformFile) where

import Prelude hiding (putStr, putStrLn, concat, readFile)
import Text.XML (Document, writeFile, def)
import Data.Text (Text, concat, pack)
import Data.Text.IO (putStr, putStrLn)
import Control.Ellipsis ((…))

import Lex
import LineTypes
import Snip
import Paragraphs
import GenerateHTML
import Directory (FPath(..), readFile, youngerThan)

writeFile' :: FPath → Document → IO ()
writeFile' (FPath p) = Text.XML.writeFile def p

transform :: Text → Document
transform = tokens … typedLines … snip … paragraphs … html

transformFile :: Bool → Bool → FPath → FPath → IO ()
transformFile force verbose i o = do
  filesystemSaysYes ← catch (i `youngerThan` o) (\_ → return True) -- try the transform if exception thrown.
  if force ∨ filesystemSaysYes then do
      putStr $ concat ["'", pack $ show i, "' ⇒ '", pack $ show o, "' "]
      contents ← readFile i
      writeFile' o (transform contents)
      putStrLn "✔"
    else
      if verbose then (putStrLn $ concat ["‹skip '", pack $ show i, "'›"]) else return ()