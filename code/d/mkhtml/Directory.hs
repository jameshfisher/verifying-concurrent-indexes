{-# LANGUAGE UnicodeSyntax, TypeSynonymInstances #-}

module Directory (FPath(..), DPath(..), Path, Path'(..), fromFilePath, getDirectoryContents, readFile, isFileType, appendSuffix, youngerThan) where

------------

import Prelude hiding (readFile, writeFile)

import Data.Text (Text)
import qualified Data.Text.IO (readFile)

import Data.List (isSuffixOf)
import Data.Either (partitionEithers)

import Control.Monad (liftM)
import Control.Ellipsis((…))

import qualified System.FilePath ((</>))
import qualified System.Directory (getDirectoryContents, doesDirectoryExist, getModificationTime)

import System.Time (ClockTime)

------------
-- ifthenelse, with condition last.
-- The condition value is usually the last thing known, not the possible routes.

choice :: α → α → Bool  → α
choice    x   _   True  = x
choice    _   y   False = y

------------

-- sensible function replacing getDirectoryContents, removing . and ..
getDirectoryContentsOfFilePath :: FilePath → IO [FilePath]
getDirectoryContentsOfFilePath p = System.Directory.getDirectoryContents p >>= filter (`notElem` [".", ".."]) … return

------------

-- put dir/file distinction in type system.
newtype FPath = FPath FilePath deriving (Eq, Ord, Show)
newtype DPath = DPath FilePath deriving (Eq, Ord, Show)
type Path = Either DPath FPath

class Path' p where
  toPath :: p → Path
  toFilePath :: p → FilePath
  (</>) :: DPath → p → p
  getModificationTime :: p → IO ClockTime

instance Path' FPath where
  toPath = Right
  toFilePath (FPath f) = f
  (DPath p1) </> (FPath p2) = FPath $ p1 System.FilePath.</> p2
  getModificationTime (FPath p) = System.Directory.getModificationTime p

instance Path' DPath where
  toPath = Left
  toFilePath (DPath f) = f
  (DPath p1) </> (DPath p2) = DPath $ p1 System.FilePath.</> p2
  getModificationTime (DPath p) = System.Directory.getModificationTime p

instance Path' Path where
  toPath = id
  toFilePath (Left p) = toFilePath p
  toFilePath (Right p) = toFilePath p
  getModificationTime (Left p) = getModificationTime p
  getModificationTime (Right p) = getModificationTime p
  p1 </> (Left  p2) = Left  $ p1 </> p2
  p1 </> (Right p2) = Right $ p1 </> p2


-------------

fromFilePath :: FilePath → IO Path
fromFilePath p = System.Directory.doesDirectoryExist p >>=
                 choice (Left $ DPath p) (Right $ FPath p) … return

------------
-- Directory operations

getDirectoryContents' :: DPath → IO [Path]
getDirectoryContents' (DPath name) = getDirectoryContentsOfFilePath name >>= mapM ((name System.FilePath.</>) … fromFilePath)

getDirectoryContents :: DPath → IO ([DPath], [FPath])
getDirectoryContents = getDirectoryContents' … liftM partitionEithers

------------
-- File operations

readFile :: FPath → IO Text
readFile (FPath p) = Data.Text.IO.readFile p

isFileType :: String → FPath → Bool
isFileType s (FPath p) = isSuffixOf ("." ++ s) p

appendSuffix :: String → FPath → FPath
appendSuffix s (FPath p) = FPath $ p ++ "." ++ s

------------

youngerThan :: (Path' a, Path' b) ⇒ a → b → IO Bool
youngerThan a b = do
  aTime ← getModificationTime a
  bTime ← getModificationTime b
  return $ aTime > bTime -- > == younger than
