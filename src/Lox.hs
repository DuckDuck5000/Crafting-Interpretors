{-# LANGUAGE OverloadedStrings #-}
module Lox 
  ( runFile
  , runPrompt
  ) where

import System.IO (hFlush, stdout, isEOF)
import System.Exit (exitWith, ExitCode(..))
import Data.IORef
import Control.Monad (when)

import Scanner (scanTokens)
import Token (Token(..), TokenType(..))

runFile :: IORef Bool -> FilePath -> IO ()
runFile hadErrorRef path = do
  source <- readFile path
  run hadErrorRef source
  hadError <- readIORef hadErrorRef
  when hadError (exitWith (ExitFailure 65))

runPrompt :: IORef Bool -> IO ()
runPrompt hadErrorRef = do
  putStr "> "
  hFlush stdout
  maybeLine <- safeGetLine
  case maybeLine of
    Nothing -> return () -- EOF
    Just line -> do
      run hadErrorRef line
      writeIORef hadErrorRef False
      runPrompt hadErrorRef
  where
    safeGetLine = do
      eof <- isEOF
      if eof
        then return Nothing
        else Just <$> getLine

run :: IORef Bool -> String -> IO ()
run hadErrorRef source = do
  putStrLn ("Running: " ++ source)
  let tokens = scanTokens source
  mapM_ print tokens
  -- If scanning finds an error, you can call reportError here as needed.

reportError :: IORef Bool -> Int -> String -> IO ()
reportError hadErrorRef line msg = do
  putStrLn $ "[line " ++ show line ++ "] Error: " ++ msg
  writeIORef hadErrorRef True
