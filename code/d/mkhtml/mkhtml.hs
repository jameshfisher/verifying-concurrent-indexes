{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}

module Main where

import Control.Monad (forM)
import Control.Ellipsis ((…))

import Directory (DPath(..), isFileType, appendSuffix, (</>))
import Files (filesOf, tree)
import Config (Config(..), getConfig)
import Transform (transformFile)

main :: IO [()]
main = do
  config ← getConfig
  let transformFile' = transformFile (configForce config) (configVerbose config)
  files ← tree (DPath ".") >>= filesOf … (filter $ isFileType "d") … return
  forM files $ \file → transformFile' file ((DPath "html") </> (appendSuffix "html" file))