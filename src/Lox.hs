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
import Parser (parse)
import Interpreter (interpret, createInterpreter, InterpreterState)
import Token (Token(..), TokenType(..))

runFile :: IORef Bool -> FilePath -> IO ()
runFile hadErrorRef path = do
  source <- readFile path
  run hadErrorRef source
  hadError <- readIORef hadErrorRef
  when hadError (exitWith (ExitFailure 65))

runPrompt :: IORef Bool -> IO ()
runPrompt hadErrorRef = do
  -- Create interpreter state once and reuse it
  interpreter <- createInterpreter hadErrorRef
  replLoop hadErrorRef interpreter
  where
    replLoop hadErrorRef interpreter = do
      putStr "> "
      hFlush stdout
      maybeLine <- safeGetLine
      case maybeLine of
        Nothing -> return () -- EOF
        Just line -> do
          runWithInterpreter hadErrorRef interpreter line
          writeIORef hadErrorRef False
          replLoop hadErrorRef interpreter
      where
        safeGetLine = do
          eof <- isEOF
          if eof
            then return Nothing
            else Just <$> getLine

run :: IORef Bool -> String -> IO ()
run hadErrorRef source = do
  -- Step 1: Scan tokens
  let tokens = scanTokens source
  
  -- Step 2: Parse tokens into AST
  parseResult <- parse hadErrorRef tokens
  case parseResult of
    Left errorMsg -> do
      putStrLn $ "Parse error: " ++ errorMsg
      writeIORef hadErrorRef True
    Right statements -> do
      -- Step 3: Create interpreter and execute
      interpreter <- createInterpreter hadErrorRef
      interpret interpreter statements

runWithInterpreter :: IORef Bool -> InterpreterState -> String -> IO ()
runWithInterpreter hadErrorRef interpreter source = do
  -- Step 1: Scan tokens
  let tokens = scanTokens source
  
  -- Step 2: Parse tokens into AST
  parseResult <- parse hadErrorRef tokens
  case parseResult of
    Left errorMsg -> do
      putStrLn $ "Parse error: " ++ errorMsg
      writeIORef hadErrorRef True
    Right statements -> do
      -- Step 3: Execute with existing interpreter state
      interpret interpreter statements

reportError :: IORef Bool -> Int -> String -> IO ()
reportError hadErrorRef line msg = do
  putStrLn $ "[line " ++ show line ++ "] Error: " ++ msg
  writeIORef hadErrorRef True
