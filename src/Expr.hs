module Expr where

import Token (Token, TokenType)

-- Expression types
data Expr 
  = Binary Expr Token Expr       -- e.g., 1 + 2
  | Grouping Expr                -- e.g., (expression)
  | Literal LiteralValue         -- e.g., 123, "string", true
  | Unary Token Expr            -- e.g., -123, !true
  | Variable Token              -- e.g., variable reference
  | Assign Token Expr           -- e.g., variable assignment
  | Logical Expr Token Expr     -- e.g., and, or
  | Call Expr Token [Expr]      -- e.g., function calls
  | Get Expr Token              -- e.g., obj.property
  | Set Expr Token Expr         -- e.g., obj.property = value
  | This Token                  -- e.g., this keyword
  | Super Token Token           -- e.g., super.method
  deriving (Show, Eq)

data LiteralValue
  = StringValue String
  | NumberValue Double
  | BoolValue Bool
  | NilValue
  deriving (Show, Eq)

-- Statement types
data Stmt
  = Expression Expr                      -- Expression statement
  | Print Expr                           -- Print statement
  | Var Token (Maybe Expr)               -- Variable declaration
  | Block [Stmt]                         -- Block statement
  | If Expr Stmt (Maybe Stmt)            -- If statement
  | While Expr Stmt                      -- While loop
  | For (Maybe Stmt) (Maybe Expr) (Maybe Expr) Stmt -- For loop
  | Function Token [Token] [Stmt]        -- Function declaration
  | Return Token (Maybe Expr)            -- Return statement
  | Class Token (Maybe Expr) [Stmt]      -- Class declaration
  deriving (Show, Eq)
