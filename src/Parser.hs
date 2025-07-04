{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad (when, mplus, mzero)
import Data.IORef

import qualified Token as T
import qualified Expr as E

-- Parser state
data ParserState = ParserState
  { tokens :: [T.Token]
  , current :: Int
  , hadErrorRef :: IORef Bool
  } deriving (Eq)

type Parser = StateT ParserState (ExceptT String IO)

-- Parse error
data ParseError = ParseError T.Token String deriving (Show)

-- Main parsing function
parse :: IORef Bool -> [T.Token] -> IO (Either String [E.Stmt])
parse hadErrorRef toks = do
  let initialState = ParserState toks 0 hadErrorRef
  result <- runExceptT $ evalStateT parseProgram initialState
  return result

parseProgram :: Parser [E.Stmt]
parseProgram = do
  statements <- many declaration
  return statements
  where
    many p = do
      finished <- isAtEnd
      if finished
        then return []
        else do
          stmt <- p
          rest <- many p
          return (stmt : rest)

-- Declarations
declaration :: Parser E.Stmt
declaration = do
  current <- peek
  case T.tokenType current of
    T.Class -> classDeclaration
    T.Fun -> funDeclaration  
    T.Var -> varDeclaration
    _ -> statement

classDeclaration :: Parser E.Stmt
classDeclaration = do
  consume T.Class "Expected 'class'"
  name <- consume' (isIdentifier) "Expected class name"
  
  superclass <- do
    less <- match [T.Less]
    if less
      then do
        super <- consume' isIdentifier "Expected superclass name"
        return $ Just (E.Variable super)
      else return Nothing
  
  consume T.LeftBrace "Expected '{' before class body"
  methods <- many funDeclaration
  consume T.RightBrace "Expected '}' after class body"
  
  return $ E.Class name superclass methods
  where
    many p = do
      finished <- check T.RightBrace
      if finished
        then return []
        else do
          method <- p
          rest <- many p
          return (method : rest)

funDeclaration :: Parser E.Stmt
funDeclaration = do
  consume T.Fun "Expected 'fun'"
  name <- consume' isIdentifier "Expected function name"
  consume T.LeftParen "Expected '(' after function name"
  
  parameters <- parseParameters
  consume T.RightParen "Expected ')' after parameters"
  body <- blockStatement
  
  case body of
    E.Block stmts -> return $ E.Function name parameters stmts
    _ -> throwError "Expected block for function body"

parseParameters :: Parser [T.Token]
parseParameters = do
  first <- check T.RightParen
  if first
    then return []
    else do
      param <- consume' isIdentifier "Expected parameter name"
      rest <- parseRestParameters
      return (param : rest)
  where
    parseRestParameters = do
      hasComma <- match [T.Comma]
      if hasComma
        then do
          param <- consume' isIdentifier "Expected parameter name"
          rest <- parseRestParameters
          return (param : rest)
        else return []

varDeclaration :: Parser E.Stmt
varDeclaration = do
  consume T.Var "Expected 'var'"
  name <- consume' isIdentifier "Expected variable name"
  
  initializer <- do
    hasEqual <- match [T.Equal]
    if hasEqual
      then do
        expr <- expression
        return $ Just expr
      else return Nothing
  
  consume T.Semicolon "Expected ';' after variable declaration"
  return $ E.Var name initializer

-- Statements
statement :: Parser E.Stmt
statement = 
  printStatement <|> 
  returnStatement <|> 
  whileStatement <|> 
  forStatement <|> 
  ifStatement <|> 
  blockStatement <|> 
  expressionStatement
  where (<|>) = mplus

printStatement :: Parser E.Stmt
printStatement = do
  consume T.Print "Expected 'print'"
  expr <- expression
  consume T.Semicolon "Expected ';' after value"
  return $ E.Print expr

returnStatement :: Parser E.Stmt
returnStatement = do
  keyword <- consume' (== T.Return) "Expected 'return'"
  
  value <- do
    semicolon <- check T.Semicolon
    if semicolon
      then return Nothing
      else do
        expr <- expression
        return $ Just expr
  
  consume T.Semicolon "Expected ';' after return value"
  return $ E.Return keyword value

whileStatement :: Parser E.Stmt
whileStatement = do
  consume T.While "Expected 'while'"
  consume T.LeftParen "Expected '(' after 'while'"
  condition <- expression
  consume T.RightParen "Expected ')' after condition"
  body <- statement
  return $ E.While condition body

forStatement :: Parser E.Stmt
forStatement = do
  consume T.For "Expected 'for'"
  consume T.LeftParen "Expected '(' after 'for'"
  
  initializer <- do
    semicolon <- match [T.Semicolon]
    if semicolon
      then return Nothing
      else do
        varDecl <- match [T.Var]
        if varDecl
          then do
            name <- consume' isIdentifier "Expected variable name"
            init <- do
              hasEqual <- match [T.Equal]
              if hasEqual
                then do
                  expr <- expression
                  return $ Just expr
                else return Nothing
            consume T.Semicolon "Expected ';' after variable declaration"
            return $ Just (E.Var name init)
          else do
            stmt <- expressionStatement
            return $ Just stmt
  
  condition <- do
    semicolon <- check T.Semicolon
    if semicolon
      then return Nothing
      else do
        expr <- expression
        return $ Just expr
  consume T.Semicolon "Expected ';' after loop condition"
  
  increment <- do
    rightParen <- check T.RightParen
    if rightParen
      then return Nothing
      else do
        expr <- expression
        return $ Just expr
  consume T.RightParen "Expected ')' after for clauses"
  
  body <- statement
  return $ E.For initializer condition increment body

ifStatement :: Parser E.Stmt
ifStatement = do
  consume T.If "Expected 'if'"
  consume T.LeftParen "Expected '(' after 'if'"
  condition <- expression
  consume T.RightParen "Expected ')' after if condition"
  
  thenBranch <- statement
  elseBranch <- do
    hasElse <- match [T.Else]
    if hasElse
      then do
        branch <- statement
        return $ Just branch
      else return Nothing
  
  return $ E.If condition thenBranch elseBranch

blockStatement :: Parser E.Stmt
blockStatement = do
  consume T.LeftBrace "Expected '{'"
  statements <- many declaration
  consume T.RightBrace "Expected '}'"
  return $ E.Block statements
  where
    many p = do
      finished <- check T.RightBrace
      if finished
        then return []
        else do
          stmt <- p
          rest <- many p
          return (stmt : rest)

expressionStatement :: Parser E.Stmt
expressionStatement = do
  expr <- expression
  consume T.Semicolon "Expected ';' after expression"
  return $ E.Expression expr

-- Expressions (precedence climbing)
expression :: Parser E.Expr
expression = assignment

assignment :: Parser E.Expr
assignment = do
  expr <- logicalOr
  
  equal <- match [T.Equal]
  if equal
    then do
      value <- assignment
      case expr of
        E.Variable name -> return $ E.Assign name value
        E.Get object name -> return $ E.Set object name value
        _ -> throwError "Invalid assignment target"
    else return expr

logicalOr :: Parser E.Expr
logicalOr = do
  expr <- logicalAnd
  
  let loop acc = do
        op <- matchAny [T.Or]
        case op of
          Just operator -> do
            right <- logicalAnd
            loop $ E.Logical acc operator right
          Nothing -> return acc
  
  loop expr

logicalAnd :: Parser E.Expr
logicalAnd = do
  expr <- equality
  
  let loop acc = do
        op <- matchAny [T.And]
        case op of
          Just operator -> do
            right <- equality
            loop $ E.Logical acc operator right
          Nothing -> return acc
  
  loop expr

equality :: Parser E.Expr
equality = do
  expr <- comparison
  
  let loop acc = do
        op <- matchAny [T.BangEqual, T.EqualEqual]
        case op of
          Just operator -> do
            right <- comparison
            loop $ E.Binary acc operator right
          Nothing -> return acc
  
  loop expr

comparison :: Parser E.Expr
comparison = do
  expr <- term
  
  let loop acc = do
        op <- matchAny [T.Greater, T.GreaterEqual, T.Less, T.LessEqual]
        case op of
          Just operator -> do
            right <- term
            loop $ E.Binary acc operator right
          Nothing -> return acc
  
  loop expr

term :: Parser E.Expr
term = do
  expr <- factor
  
  let loop acc = do
        op <- matchAny [T.Minus, T.Plus]
        case op of
          Just operator -> do
            right <- factor
            loop $ E.Binary acc operator right
          Nothing -> return acc
  
  loop expr

factor :: Parser E.Expr
factor = do
  expr <- unary
  
  let loop acc = do
        op <- matchAny [T.Slash, T.Star]
        case op of
          Just operator -> do
            right <- unary
            loop $ E.Binary acc operator right
          Nothing -> return acc
  
  loop expr

unary :: Parser E.Expr
unary = do
  op <- matchAny [T.Bang, T.Minus]
  case op of
    Just operator -> do
      right <- unary
      return $ E.Unary operator right
    Nothing -> call

call :: Parser E.Expr
call = do
  expr <- primary
  
  let loop acc = do
        leftParen <- match [T.LeftParen]
        if leftParen
          then do
            args <- arguments
            paren <- consume' (== T.RightParen) "Expected ')' after arguments"
            loop $ E.Call acc paren args
          else do
            dot <- match [T.Dot]
            if dot
              then do
                name <- consume' isIdentifier "Expected property name after '.'"
                loop $ E.Get acc name
              else return acc
  
  loop expr

arguments :: Parser [E.Expr]
arguments = do
  rightParen <- check T.RightParen
  if rightParen
    then return []
    else do
      first <- expression
      rest <- parseRestArguments
      return (first : rest)
  where
    parseRestArguments = do
      hasComma <- match [T.Comma]
      if hasComma
        then do
          arg <- expression
          rest <- parseRestArguments
          return (arg : rest)
        else return []

primary :: Parser E.Expr
primary = do
  -- Try false
  false <- match [T.False_]
  if false then return $ E.Literal (E.BoolValue False)
  else do
    -- Try true
    true <- match [T.True_]
    if true then return $ E.Literal (E.BoolValue True)
    else do
      -- Try nil
      nil <- match [T.Nil]
      if nil then return $ E.Literal E.NilValue
      else do
        -- Try this
        this <- matchAny [T.This]
        case this of
          Just keyword -> return $ E.This keyword
          Nothing -> do
            -- Try super
            super <- matchAny [T.Super]
            case super of
              Just keyword -> do
                consume T.Dot "Expected '.' after 'super'"
                method <- consume' isIdentifier "Expected superclass method name"
                return $ E.Super keyword method
              Nothing -> do
                -- Try number
                tok <- peek
                case T.tokenType tok of
                  T.Number n -> do
                    advance
                    return $ E.Literal (E.NumberValue n)
                  T.StringLit s -> do
                    advance
                    return $ E.Literal (E.StringValue s)
                  T.Identifier _ -> do
                    token <- advance
                    return $ E.Variable token
                  T.LeftParen -> do
                    advance
                    expr <- expression
                    consume T.RightParen "Expected ')' after expression"
                    return $ E.Grouping expr
                  _ -> throwError $ "Unexpected token: " ++ show tok

-- Helper functions
isAtEnd :: Parser Bool
isAtEnd = do
  state <- get
  let currentIdx = current state
  let tokenList = tokens state
  return $ currentIdx >= length tokenList || 
           (currentIdx < length tokenList && T.tokenType (tokenList !! currentIdx) == T.EOF)

peek :: Parser T.Token
peek = do
  state <- get
  let currentIdx = current state
  let tokenList = tokens state
  if currentIdx >= length tokenList
    then return $ T.Token T.EOF "" 0
    else return $ tokenList !! currentIdx

previous :: Parser T.Token
previous = do
  state <- get
  let currentIdx = current state
  let tokenList = tokens state
  return $ tokenList !! (currentIdx - 1)

advance :: Parser T.Token
advance = do
  finished <- isAtEnd
  when (not finished) $ do
    state <- get
    put $ state { current = current state + 1 }
  previous

check :: T.TokenType -> Parser Bool
check tokenType = do
  finished <- isAtEnd
  if finished
    then return False
    else do
      token <- peek
      return $ tokenType == T.tokenType token

match :: [T.TokenType] -> Parser Bool
match types = do
  anyMatch <- mapM check types
  if or anyMatch
    then do
      advance
      return True
    else return False

matchAny :: [T.TokenType] -> Parser (Maybe T.Token)
matchAny types = do
  anyMatch <- mapM check types
  if or anyMatch
    then do
      token <- advance
      return $ Just token
    else return Nothing

consume :: T.TokenType -> String -> Parser T.Token
consume tokenType message = do
  isValid <- check tokenType
  if isValid
    then advance
    else do
      token <- peek
      throwError $ message ++ " at " ++ show token

consume' :: (T.TokenType -> Bool) -> String -> Parser T.Token
consume' predicate message = do
  token <- peek
  if predicate (T.tokenType token)
    then advance
    else throwError $ message ++ " at " ++ show token

isIdentifier :: T.TokenType -> Bool
isIdentifier (T.Identifier _) = True
isIdentifier _ = False
