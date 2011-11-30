{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}

module Main where

import Prelude hiding (putStr, putStrLn, concat, readFile)
import Control.Monad (forM)
import Text.XML (Document, writeFile, def)
import Data.Text (Text, concat, pack)
import Data.Text.IO (putStr, putStrLn)
import System.Environment (getArgs)
import System.Console.CmdArgs.GetOpt (OptDescr(Option), ArgOrder(Permute), ArgDescr(NoArg), getOpt)

import Control.Ellipsis ((…))
import Lex
import LineTypes
import Snip
import Paragraphs
import GenerateHTML

import Directory (DPath(..), FPath(..), isFileType, appendSuffix, (</>), readFile, youngerThan)
import Files (filesOf, tree)


writeFile' :: FPath → Document → IO ()
writeFile' (FPath p) = Text.XML.writeFile def p



transform :: Text → Document
transform = tokens … typedLines … snip … paragraphs … html

newSource :: FPath → FPath → IO Bool
newSource i o = catch (i `youngerThan` o) (\_ → return False)

transformFile :: Bool → FPath → FPath → IO ()
transformFile force i o = do
  putStr $ concat ["Transforming '", pack $ show i, "' into '", pack $ show o, "' ... "]
  filesystemSaysYes ← newSource i o
  if force || filesystemSaysYes then do
      contents ← readFile i
      writeFile' o (transform contents)
      putStrLn "done."
    else
      putStrLn "skipping."

-------------

data Arg = Force | Help
data Config = Config
  { configForce :: Bool
  , configHelp  :: Bool
  }

getConfig :: IO Config
getConfig = getArgs >>= getOpt Permute argSpec … justOpts … foldl addArg defaultConfig … return
  where
    defaultConfig = Config { configForce = False, configHelp = False }

    argSpec = [
        Option ['f'] ["force"] (NoArg Force) "Force all recompilation"
      , Option ['h'] ["help"]  (NoArg Help ) "Get help"
      ]

    addArg c arg = case arg of
      Force → c { configForce = True }
      Help  → c { configHelp = True }

    justOpts (opts, _, _) = opts

--------------

main :: IO [()]
main = do
  config ← getConfig
  let transformFile' = transformFile (configForce config)
  files ← tree (DPath ".") >>= filesOf … (filter $ isFileType "d") … return
  forM files $ \file → transformFile' file ((DPath "html") </> (appendSuffix "html" file))