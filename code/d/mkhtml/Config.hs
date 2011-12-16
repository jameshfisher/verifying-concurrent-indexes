{-# LANGUAGE UnicodeSyntax #-}

module Config (Config(..), getConfig) where

import System.Environment (getArgs)
import System.Console.CmdArgs.GetOpt (OptDescr(Option), ArgOrder(Permute), ArgDescr(NoArg), getOpt)
import Control.Ellipsis ((…))

data Arg = Force | Help | Verbose
data Config = Config
  { configForce   :: Bool
  , configHelp    :: Bool
  , configVerbose :: Bool
  }

getConfig :: IO Config
getConfig = getArgs >>= getOpt Permute argSpec … justOpts … foldl addArg defaultConfig … return
  where
    defaultConfig = Config { configForce = False, configHelp = False, configVerbose = False }

    argSpec =
      [ Option ['f'] ["force"]   (NoArg Force  ) "Force all recompilation"
      , Option ['h'] ["help"]    (NoArg Help   ) "Get help"
      , Option ['v'] ["verbose"] (NoArg Verbose) "Be verbose"
      ]

    addArg c arg = case arg of
      Force   → c { configForce   = True }
      Help    → c { configHelp    = True }
      Verbose → c { configVerbose = True }

    justOpts (opts, _, _) = opts
