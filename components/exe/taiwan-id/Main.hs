module Main (main) where

import Control.Monad.Random
  ( evalRandIO
  )
import System.Environment
  ( getArgs
  )
import System.Exit
  ( exitFailure
  , exitSuccess
  )
import System.IO
  ( stderr
  , stdout
  )
import Taiwan.ID.CLI
  ( CommandLineResult (CommandLineFailure, CommandLineSuccess)
  )

import qualified Data.Text.IO as TIO
import qualified Taiwan.ID.CLI as CLI

main :: IO ()
main = do
  args <- getArgs
  result <- evalRandIO (CLI.run args)
  case result of
    CommandLineSuccess ls -> mapM_ (TIO.hPutStrLn stdout) ls >> exitSuccess
    CommandLineFailure ls -> mapM_ (TIO.hPutStrLn stderr) ls >> exitFailure
