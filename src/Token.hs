module Token (Token(..), TokenType(..)) where
    
data TokenType
  = LeftParen | RightParen | Comma | Dot | Minus | Plus | Semicolon | Slash | Star
  | Bang | BangEqual | Equal | EqualEqual | Greater | GreaterEqual
  | Less | LessEqual
  | Identifier String
  | StringLit String
  | Number Double
  -- Keywords
  | And | Class | Else | False_ | Fun | For | If | Nil | Or
  | Print | Return | Super | This | True_ | Var | While
  | EOF
  deriving (Show, Eq)

data Token = Token
  { tokenType :: TokenType
  , lexeme    :: String
  , lineNum   :: Int
  } deriving (Show, Eq)
