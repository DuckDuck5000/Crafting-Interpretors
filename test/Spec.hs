{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Test.Hspec
import Control.Exception (try, SomeException)
import Control.Monad (when)
import Data.IORef
import Lox (runFile)

-- Helper function to test if a Lox file runs without throwing exceptions
testLoxFile :: String -> IO Bool
testLoxFile filename = do
  hadErrorRef <- newIORef False
  result <- try $ runFile hadErrorRef filename
  case result of
    Left (ex :: SomeException) -> do
      putStrLn $ "Exception in " ++ filename ++ ": " ++ show ex
      return False
    Right _ -> do
      hadError <- readIORef hadErrorRef
      when hadError $ putStrLn $ "Lox error in " ++ filename
      return (not hadError)  -- Success if no errors

main :: IO ()
main = hspec $ do
  describe "Basic Tests" $ do
    it "can run a simple test" $ do
      (1 + 1) `shouldBe` 2
      
    it "can handle strings" $ do
      "hello" `shouldBe` "hello"

  describe "Basic Lox Tests" $ do
    it "can run the debug.lox file" $ do
      success <- testLoxFile "examples/debug.lox"
      success `shouldBe` True
      
    it "can run the simple.lox file" $ do
      success <- testLoxFile "examples/simple.lox"
      success `shouldBe` True

  describe "Example File Tests" $ do
    it "can run hello.lox without errors" $ do
      success <- testLoxFile "examples/hello.lox"
      success `shouldBe` True

    it "can run test_expressions.lox without errors" $ do
      success <- testLoxFile "examples/test_expressions.lox"
      success `shouldBe` True

    it "can run test_functions.lox without errors" $ do
      success <- testLoxFile "examples/test_functions.lox"
      success `shouldBe` True

    it "can run test_control_flow.lox without errors" $ do
      success <- testLoxFile "examples/test_control_flow.lox"
      success `shouldBe` True
