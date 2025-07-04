module Lox.Types where

import Data.IORef
import qualified Data.Map as Map

-- Runtime value representation
data LoxValue
  = LoxString String
  | LoxNumber Double
  | LoxBool Bool
  | LoxNil
  | LoxCallable LoxCallable
  deriving (Eq)

data LoxCallable
  = LoxFunction String [String] [Stmt] (IORef Environment) -- name, params, body, closure
  | LoxBuiltin String Int (IORef Environment -> [LoxValue] -> IO LoxValue) -- name, arity, function

-- Forward declarations to avoid circular imports
data Environment = Environment
  { values :: Map.Map String Value
  , enclosing :: Maybe (IORef Environment)
  }

data Value
  = StringVal String
  | NumberVal Double
  | BoolVal Bool
  | NilVal
  | FunctionVal String [String] String
  deriving (Eq, Show)

-- Forward declaration for statements
data Stmt = StubStmt  -- Will be imported from AST module

instance Eq LoxCallable where
  (LoxFunction n1 _ _ _) == (LoxFunction n2 _ _ _) = n1 == n2
  (LoxBuiltin n1 _ _) == (LoxBuiltin n2 _ _) = n1 == n2
  _ == _ = False

instance Show LoxValue where
  show (LoxString s) = s
  show (LoxNumber n) = 
    if fromIntegral (round n) == n
      then show (round n)
      else show n
  show (LoxBool True) = "true"
  show (LoxBool False) = "false"
  show LoxNil = "nil"
  show (LoxCallable (LoxFunction name _ _ _)) = "<fn " ++ name ++ ">"
  show (LoxCallable (LoxBuiltin name _ _)) = "<builtin " ++ name ++ ">"
