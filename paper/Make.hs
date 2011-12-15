{-# LANGUAGE UnicodeSyntax, GADTs #-}

module Make where

import System.Cmd (rawSystem)
import GHC.IO.Exception (ExitCode(ExitSuccess))
import qualified Data.Map (Map, insert, empty)
import qualified Data.Set (Set, insert, empty)

doSystem :: String → [String] → IO Bool
doSystem prog args = do
  putStrLn $ show (prog, args)
  exitCode ← rawSystem prog args
  case exitCode of
    ExitSuccess → putStrLn " ✔" >> return True
    _           → putStrLn " X" >> return False


class Worker w where
  target :: w → FilePath
  dependencies :: w → [FilePath]
  make  :: w → IO Bool

  vertex' :: w → (FilePath, [FilePath])
  vertex' w = (target w, dependencies w)


data I where
  I :: (Worker w)⇒ w → I

depsMap :: [I] → Map FilePath I
depsMap = foldl (\m i@(I w) → Data.Map.insert (target w) i m) Data.Map.empty 

make :: (Data.Map.Map FilePath I) → Data.Set.Set FilePath → FilePath → IO Bool
make m i f = if member f i then return False else case lookup f m of
  Nothing → return False
  Just (I w) → map (make m ignore') (dependencies w)
    where
      ignore' = Data.Set.insert f i

runDeps :: [I] → IO ()
runDeps d = putStrLn $ show $ fst $ graphOf d


-------------


data XSLT = XSLT
  { xsltTarg :: FilePath
  , xsltSty  :: FilePath
  , xsltSrc  :: FilePath
  }

instance Worker XSLT where
  target = xsltTarg
  dependencies (XSLT _ sty src) = [sty, src]
  make  (XSLT targ sty src) = doSystem "xsltproc" ["--xinclude", sty, src, "--output", targ]

deps = 
  [ I $ XSLT "tmp.html" "paper.html" "macro.xsl"
  , I $ XSLT "out.html" "tmp.html"   "transform.xsl"
  ]

main = runDeps deps  