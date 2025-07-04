{-# LANGUAGE OverloadedStrings #-}
module Interpreter where

import Control.Monad.State
import Control.Monad.Except
import Control.Exception (try, IOException, Exception, throwIO, catch)
import Data.IORef
import qualified Data.Map as Map
import Data.Maybe (isJust, fromJust)
import System.IO.Unsafe (unsafePerformIO)
import Data.Typeable

import qualified Token as T
import qualified Expr as E
import Environment (Environment, Value(..), createEnvironment, define, assign)
import qualified Environment as Env

-- Exception for handling return statements
data ReturnException = ReturnException LoxValue deriving (Show, Typeable)
instance Exception ReturnException

-- Runtime value representation
data LoxValue
  = LoxString String
  | LoxNumber Double
  | LoxBool Bool
  | LoxNil
  | LoxCallable LoxCallable
  deriving (Eq)

data LoxCallable
  = LoxFunction String [String] [E.Stmt] (IORef Environment) -- name, params, body, closure
  | LoxBuiltin String Int (IORef Environment -> [LoxValue] -> IO LoxValue) -- name, arity, function

instance Eq LoxCallable where
  (LoxFunction n1 _ _ _) == (LoxFunction n2 _ _ _) = n1 == n2
  (LoxBuiltin n1 _ _) == (LoxBuiltin n2 _ _) = n1 == n2
  _ == _ = False

-- Global function registry to store actual function objects
{-# NOINLINE globalFunctions #-}
globalFunctions :: IORef (Map.Map String LoxCallable)
globalFunctions = unsafePerformIO $ newIORef Map.empty

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

-- Convert between LoxValue and Environment.Value
convertToValue :: LoxValue -> Value
convertToValue (LoxString s) = StringVal s
convertToValue (LoxNumber n) = NumberVal n
convertToValue (LoxBool b) = BoolVal b
convertToValue LoxNil = NilVal
convertToValue (LoxCallable func) = 
  let LoxFunction name params _ _ = func
      paramNames = params
      bodyId = name ++ "_body"  -- Simple ID for now
  in FunctionVal name paramNames bodyId

convertFromValue :: Value -> LoxValue
convertFromValue (StringVal s) = LoxString s
convertFromValue (NumberVal n) = LoxNumber n
convertFromValue (BoolVal b) = LoxBool b
convertFromValue NilVal = LoxNil
convertFromValue (FunctionVal name params bodyId) = 
  -- Look up the function in the global registry
  unsafePerformIO $ do
    funcs <- readIORef globalFunctions
    case Map.lookup name funcs of
      Just callable -> return $ LoxCallable callable
      Nothing -> return LoxNil  -- Function not found

-- Interpreter state
data InterpreterState = InterpreterState
  { environment :: IORef Environment
  , globals :: IORef Environment
  , hadErrorRef :: IORef Bool
  }

type Interpreter = StateT InterpreterState IO

-- Runtime errors
data RuntimeError = RuntimeError T.Token String deriving (Show)

-- Create initial interpreter state
createInterpreter :: IORef Bool -> IO InterpreterState
createInterpreter hadErrorRef = do
  globalsRef <- newIORef =<< createEnvironment Nothing
  envRef <- newIORef =<< createEnvironment Nothing
  
  -- Add built-in functions
  builtins <- createBuiltins globalsRef
  mapM_ (\(name, func) -> define globalsRef name (convertToValue func)) builtins
  
  return $ InterpreterState envRef globalsRef hadErrorRef

createBuiltins :: IORef Environment -> IO [(String, LoxValue)]
createBuiltins globalsRef = do
  return 
    [ ("clock", LoxCallable $ LoxBuiltin "clock" 0 clockFunction)
    ]
  where
    clockFunction _ _ = do
      -- Simple clock implementation - return current time in seconds
      -- In a real implementation, you'd use System.Clock or similar
      return $ LoxNumber 0.0

-- Main interpretation functions
interpret :: InterpreterState -> [E.Stmt] -> IO ()
interpret state statements = do
  result <- runStateT (mapM_ executeStatement statements) state
  return ()

executeStatement :: E.Stmt -> Interpreter ()
executeStatement stmt = case stmt of
  E.Expression expr -> do
    value <- evaluateExpression expr
    return ()
  
  E.Print expr -> do
    value <- evaluateExpression expr
    liftIO $ putStrLn $ show value
  
  E.Var token maybeExpr -> do
    value <- case maybeExpr of
      Nothing -> return LoxNil
      Just expr -> evaluateExpression expr
    
    state <- get
    let name = case T.tokenType token of
          T.Identifier n -> n
          _ -> "unknown"
    liftIO $ define (environment state) name (convertToValue value)
  
  E.Block statements -> do
    state <- get
    -- Create new environment for block scope
    parentEnv <- liftIO $ readIORef (environment state)
    newEnv <- liftIO $ createEnvironment (Just (environment state))
    newEnvRef <- liftIO $ newIORef newEnv
    
    -- Execute block with new environment
    put $ state { environment = newEnvRef }
    mapM_ executeStatement statements
    
    -- Restore previous environment
    put state
  
  E.If condition thenBranch elseBranch -> do
    conditionValue <- evaluateExpression condition
    if isTruthy conditionValue
      then executeStatement thenBranch
      else case elseBranch of
        Just elseStmt -> executeStatement elseStmt
        Nothing -> return ()
  
  E.While condition body -> do
    let loop = do
          conditionValue <- evaluateExpression condition
          if isTruthy conditionValue
            then do
              executeStatement body
              loop
            else return ()
    loop
  
  E.For initializer condition increment body -> do
    -- Create new scope for for loop
    state <- get
    parentEnv <- liftIO $ readIORef (environment state)
    newEnv <- liftIO $ createEnvironment (Just (environment state))
    newEnvRef <- liftIO $ newIORef newEnv
    put $ state { environment = newEnvRef }
    
    -- Execute initializer
    case initializer of
      Just init -> executeStatement init
      Nothing -> return ()
    
    -- Loop
    let loop = do
          -- Check condition
          conditionValue <- case condition of
            Just cond -> evaluateExpression cond
            Nothing -> return $ LoxBool True
          
          if isTruthy conditionValue
            then do
              executeStatement body
              case increment of
                Just inc -> do
                  _ <- evaluateExpression inc
                  return ()
                Nothing -> return ()
              loop
            else return ()
    loop
    
    -- Restore environment
    put state
  
  E.Function name params body -> do
    state <- get
    let paramNames = map (\t -> case T.tokenType t of
                              T.Identifier n -> n
                              _ -> "unknown") params
    let funcName = case T.tokenType name of
                     T.Identifier n -> n
                     _ -> "unknown"
    
    -- Create a placeholder closure that will be updated
    closure <- liftIO $ readIORef (environment state)
    closureRef <- liftIO $ newIORef closure
    let function = LoxFunction funcName paramNames body closureRef
    
    -- Define the function in the current environment first
    liftIO $ define (environment state) funcName (convertToValue (LoxCallable function))
    
    -- Update the closure to include the function itself for recursion
    updatedClosure <- liftIO $ readIORef (environment state)
    liftIO $ writeIORef closureRef updatedClosure
    
    -- Store the function in the global registry
    liftIO $ do
      funcs <- readIORef globalFunctions
      writeIORef globalFunctions $ Map.insert funcName function funcs
  
  E.Return _ maybeExpr -> do
    value <- case maybeExpr of
      Nothing -> return LoxNil
      Just expr -> evaluateExpression expr
    -- Throw a return exception to unwind the call stack
    liftIO $ throwIO $ ReturnException value
  
  E.Class name superclass methods -> do
    -- Implementation for classes would go here
    return ()

evaluateExpression :: E.Expr -> Interpreter LoxValue
evaluateExpression expr = case expr of
  E.Literal (E.StringValue s) -> return $ LoxString s
  E.Literal (E.NumberValue n) -> return $ LoxNumber n
  E.Literal (E.BoolValue b) -> return $ LoxBool b
  E.Literal E.NilValue -> return LoxNil
  
  E.Grouping expr -> evaluateExpression expr
  
  E.Unary operator right -> do
    rightValue <- evaluateExpression right
    case T.tokenType operator of
      T.Minus -> case rightValue of
        LoxNumber n -> return $ LoxNumber (-n)
        _ -> runtimeError operator "Operand must be a number"
      T.Bang -> return $ LoxBool (not $ isTruthy rightValue)
      _ -> runtimeError operator "Invalid unary operator"
  
  E.Binary left operator right -> do
    leftValue <- evaluateExpression left
    rightValue <- evaluateExpression right
    
    case T.tokenType operator of
      T.Plus -> case (leftValue, rightValue) of
        (LoxNumber l, LoxNumber r) -> return $ LoxNumber (l + r)
        (LoxString l, LoxString r) -> return $ LoxString (l ++ r)
        -- String concatenation with automatic conversion
        (LoxString l, r) -> return $ LoxString (l ++ show r)
        (l, LoxString r) -> return $ LoxString (show l ++ r)
        _ -> runtimeError operator "Operands must be two numbers or at least one string"
      
      T.Minus -> case (leftValue, rightValue) of
        (LoxNumber l, LoxNumber r) -> return $ LoxNumber (l - r)
        _ -> runtimeError operator "Operands must be numbers"
      
      T.Star -> case (leftValue, rightValue) of
        (LoxNumber l, LoxNumber r) -> return $ LoxNumber (l * r)
        _ -> runtimeError operator "Operands must be numbers"
      
      T.Slash -> case (leftValue, rightValue) of
        (LoxNumber l, LoxNumber r) -> 
          if r == 0
            then runtimeError operator "Division by zero"
            else return $ LoxNumber (l / r)
        _ -> runtimeError operator "Operands must be numbers"
      
      T.Greater -> case (leftValue, rightValue) of
        (LoxNumber l, LoxNumber r) -> return $ LoxBool (l > r)
        _ -> runtimeError operator "Operands must be numbers"
      
      T.GreaterEqual -> case (leftValue, rightValue) of
        (LoxNumber l, LoxNumber r) -> return $ LoxBool (l >= r)
        _ -> runtimeError operator "Operands must be numbers"
      
      T.Less -> case (leftValue, rightValue) of
        (LoxNumber l, LoxNumber r) -> return $ LoxBool (l < r)
        _ -> runtimeError operator "Operands must be numbers"
      
      T.LessEqual -> case (leftValue, rightValue) of
        (LoxNumber l, LoxNumber r) -> return $ LoxBool (l <= r)
        _ -> runtimeError operator "Operands must be numbers"
      
      T.EqualEqual -> return $ LoxBool (isEqual leftValue rightValue)
      T.BangEqual -> return $ LoxBool (not $ isEqual leftValue rightValue)
      
      _ -> runtimeError operator "Invalid binary operator"
  
  E.Variable name -> do
    state <- get
    let varName = case T.tokenType name of
          T.Identifier n -> n
          _ -> "unknown"
    maybeValue <- liftIO $ Env.get (environment state) varName
    case maybeValue of
      Just value -> return $ convertFromValue value
      Nothing -> runtimeError name $ "Undefined variable '" ++ varName ++ "'"
  
  E.Assign name value -> do
    val <- evaluateExpression value
    state <- get
    let varName = case T.tokenType name of
          T.Identifier n -> n
          _ -> "unknown"
    success <- liftIO $ assign (environment state) varName (convertToValue val)
    if success
      then return val
      else runtimeError name $ "Undefined variable '" ++ varName ++ "'"
  
  E.Logical left operator right -> do
    leftValue <- evaluateExpression left
    case T.tokenType operator of
      T.Or -> if isTruthy leftValue
              then return leftValue
              else evaluateExpression right
      T.And -> if not $ isTruthy leftValue
               then return leftValue
               else evaluateExpression right
      _ -> runtimeError operator "Invalid logical operator"
  
  E.Call callee paren arguments -> do
    calleeValue <- evaluateExpression callee
    argValues <- mapM evaluateExpression arguments
    
    case calleeValue of
      LoxCallable callable -> do
        let arity = getArity callable
        if length argValues /= arity
          then runtimeError paren $ "Expected " ++ show arity ++ " arguments but got " ++ show (length argValues)
          else callFunction callable argValues
      _ -> runtimeError paren "Can only call functions and classes"
  
  _ -> return LoxNil -- Placeholder for other expressions

-- Helper functions
isTruthy :: LoxValue -> Bool
isTruthy LoxNil = False
isTruthy (LoxBool b) = b
isTruthy _ = True

isEqual :: LoxValue -> LoxValue -> Bool
isEqual LoxNil LoxNil = True
isEqual (LoxBool a) (LoxBool b) = a == b
isEqual (LoxNumber a) (LoxNumber b) = a == b
isEqual (LoxString a) (LoxString b) = a == b
isEqual _ _ = False

getArity :: LoxCallable -> Int
getArity (LoxFunction _ params _ _) = length params
getArity (LoxBuiltin _ arity _) = arity

callFunction :: LoxCallable -> [LoxValue] -> Interpreter LoxValue
callFunction (LoxFunction name params body closure) arguments = do
  -- Create new environment for function scope
  parentEnv <- liftIO $ readIORef closure
  newEnv <- liftIO $ createEnvironment (Just closure)
  newEnvRef <- liftIO $ newIORef newEnv
  
  -- Bind parameters
  mapM_ (\(param, arg) -> liftIO $ define newEnvRef param (convertToValue arg)) (zip params arguments)
  
  -- Execute function body
  state <- get
  let oldEnv = environment state
  put $ state { environment = newEnvRef }
  
  result <- liftIO $ catch 
    (do 
      _ <- runStateT (mapM_ executeStatement body) state { environment = newEnvRef }
      return LoxNil  -- If no return statement, return nil
    )
    (\(ReturnException value) -> return value)
  
  -- Restore environment
  put $ state { environment = oldEnv }
  
  return result

callFunction (LoxBuiltin name arity func) arguments = do
  state <- get
  liftIO $ func (environment state) arguments

runtimeError :: T.Token -> String -> Interpreter a
runtimeError token message = do
  state <- get
  liftIO $ do
    putStrLn $ "[line " ++ show (T.lineNum token) ++ "] Runtime Error: " ++ message
    writeIORef (hadErrorRef state) True
  error message
