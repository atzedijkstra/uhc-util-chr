{-# LANGUAGE TemplateHaskell #-}

module Main where

import System.Environment
import System.Console.GetOpt
import System.Exit
import Control.Monad

import UHC.Util.Lens
import UHC.Util.CHR.Solve.TreeTrie.Examples.Term.Main

-- | Immediate quit options
data ImmQuit
  = ImmQuit_Help

-- | Program options
data Opts
  = Opts
      { _optVerbose                 :: Bool
      , _optSucceedOnLeftoverWork   :: Bool
      , _optImmQuit                 :: [ImmQuit]
      }

mkLabel ''Opts

defaultOpts :: Opts
defaultOpts
  = Opts
      { _optVerbose                 = False
      , _optSucceedOnLeftoverWork   = False
      , _optImmQuit                 = []
      }

-- | Options for 'getOpt'
options :: [OptDescr (Opts -> Opts)]
options =
    [ mk "v" ["verbose"] "extra output, debugging output"
         (NoArg $ optVerbose ^= True)
    , mk "s" ["succeed-on-leftover"] "left over unresolvable (non residue) work is also a successful result"
         (NoArg $ optSucceedOnLeftoverWork ^= True)
    , mk "h" ["help"] "print this help"
         (NoArg $ optImmQuit ^$= (ImmQuit_Help :))
    ]
  where
    mk so lo desc o = Option so lo o desc
   
main = do
    args <- getArgs
    progname <- getProgName

    case getOpt Permute options args of
       -- options ok
       (o,[fn],[]) -> do
           let opts = foldl (flip id) defaultOpts o
           case _optImmQuit opts of
             imm@(_:_) -> forM_ imm $ \iq -> case iq of
               ImmQuit_Help -> printUsage progname []

             -- no immediate quit options
             _ -> do
               flip runFile fn $ 
                 (if _optVerbose opts then [RunOpt_DebugTrace] else []) ++
                 (if _optSucceedOnLeftoverWork opts then [RunOpt_SucceedOnLeftoverWork] else [])

       (_,_,errs) -> do
           printUsage progname errs
           exitFailure

    return ()

  where
    printUsage progname errs = putStrLn $ concat errs ++ usageInfo (header progname) options
    header progname = "Usage: " ++ progname ++ " [OPTION...] file\n\nOptions:"

