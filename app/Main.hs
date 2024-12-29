module Main where

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import Data.IORef
import Control.Monad (when)
import Lox (runFile, runPrompt)

main :: IO ()
main = do
  hadErrorRef <- newIORef False
  args <- getArgs
  case args of
    [filename] -> runFile hadErrorRef filename
    []         -> runPrompt hadErrorRef
    _          -> putStrLn "Usage: hlox [script]" >> exitWith (ExitFailure 64)
