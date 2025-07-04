module Environment 
  ( Environment(..)
  , Value(..) 
  , createEnvironment
  , define
  , get
  , assign
  ) where

import qualified Data.Map as Map
import Data.IORef

-- Simple value type to avoid circular dependencies  
data Value
  = StringVal String
  | NumberVal Double
  | BoolVal Bool
  | NilVal
  | FunctionVal String [String] String  -- name, params, body_id for lookup
  deriving (Eq, Show)

-- Environment for variable storage
data Environment = Environment
  { values :: Map.Map String Value
  , enclosing :: Maybe (IORef Environment)
  }

-- Create a new environment
createEnvironment :: Maybe (IORef Environment) -> IO Environment
createEnvironment enclosing = 
  return $ Environment Map.empty enclosing

-- Define a variable in the environment
define :: IORef Environment -> String -> Value -> IO ()
define envRef name value = do
  env <- readIORef envRef
  let newValues = Map.insert name value (values env)
  writeIORef envRef $ env { values = newValues }

-- Get a variable from the environment
get :: IORef Environment -> String -> IO (Maybe Value)
get envRef name = do
  env <- readIORef envRef
  case Map.lookup name (values env) of
    Just value -> return $ Just value
    Nothing -> case enclosing env of
      Just parent -> get parent name
      Nothing -> return Nothing

-- Assign to an existing variable
assign :: IORef Environment -> String -> Value -> IO Bool
assign envRef name value = do
  env <- readIORef envRef
  if Map.member name (values env)
    then do
      let newValues = Map.insert name value (values env)
      writeIORef envRef $ env { values = newValues }
      return True
    else case enclosing env of
      Just parent -> assign parent name value
      Nothing -> return False
