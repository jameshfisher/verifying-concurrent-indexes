{-# LANGUAGE UnicodeSyntax, TypeSynonymInstances #-}

module Files (tree, filesOf) where

----------

import Prelude hiding (readFile, writeFile)
import Directory (FPath, DPath, getDirectoryContents)

-----------

data Dir = Dir DPath [Dir] [FPath] deriving (Show)


tree :: DPath → IO Dir
tree path = do
  (dirPaths, files) ← getDirectoryContents path
  dirs <- mapM tree dirPaths
  return $ Dir path dirs files

filesOf :: Dir → [FPath]
filesOf (Dir _ dirs files) = files ++ (concat $ map filesOf dirs)