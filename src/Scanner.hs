module Scanner (scanTokens) where

import Token
import Data.Char (isAlpha, isAlphaNum, isDigit)

scanTokens :: String -> [Token]
scanTokens input = tokenize input 1 1 []
  where
    tokenize :: String -> Int -> Int -> [Token] -> [Token]
    tokenize [] line _ acc = reverse (Token EOF "" line : acc)
    tokenize (c:cs) line col acc =
      case c of
        '(' -> addTok LeftParen
        ')' -> addTok RightParen
        '{' -> addTok LeftBrace
        '}' -> addTok RightBrace
        ',' -> addTok Comma
        '.' -> addTok Dot
        '-' -> addTok Minus
        '+' -> addTok Plus
        ';' -> addTok Semicolon
        '*' -> addTok Star
        '!' ->
          if peek cs == '='
            then addTokWithNext BangEqual "="
            else addTok Bang
        '=' ->
          if peek cs == '='
            then addTokWithNext EqualEqual "="
            else addTok Equal
        '<' ->
          if peek cs == '='
            then addTokWithNext LessEqual "="
            else addTok Less
        '>' ->
          if peek cs == '='
            then addTokWithNext GreaterEqual "="
            else addTok Greater
        '/' ->
          if peek cs == '/'
            then skipLineComment cs line (col + 2)
            else addTok Slash
        ' '  -> tokenize cs line (col + 1) acc
        '\r' -> tokenize cs line (col + 1) acc
        '\t' -> tokenize cs line (col + 1) acc
        '\n' -> tokenize cs (line + 1) 1 acc
        '"'  -> stringLiteral cs line (col + 1) acc
        d | isDigit d -> numberLiteral (d:cs) line col acc
        a | isAlpha a -> identifier (a:cs) line col acc
        _    -> tokenize cs line (col + 1) acc -- Ignore unexpected characters for now
      where
        peek [] = '\0'
        peek (x:_) = x

        addTok :: TokenType -> [Token]
        addTok tType =
          let newToken = Token tType [c] line
          in tokenize cs line (col + 1) (newToken : acc)

        addTokWithNext :: TokenType -> String -> [Token]
        addTokWithNext tType extra =
          let newToken = Token tType (c : extra) line
          in tokenize (drop 1 cs) line (col + 2) (newToken : acc)

        skipLineComment :: String -> Int -> Int -> [Token]
        skipLineComment rest l c = tokenize (dropWhile (/= '\n') rest) l (c + length (takeWhile (/= '\n') rest)) acc

        stringLiteral :: String -> Int -> Int -> [Token] -> [Token]
        stringLiteral s l c acc =
          let (str, rest, newLine) = consumeString s l c ""
          in if newLine
               then acc -- Handle unterminated strings as needed
               else tokenize rest l (c + length str + 2) (Token (StringLit str) ("\"" ++ str ++ "\"") l : acc)

        consumeString :: String -> Int -> Int -> String -> (String, String, Bool)
        consumeString [] _ _ _ = ("", [], True) -- Unterminated
        consumeString (x:xs) l c acc
          | x == '"' = (acc, xs, False)
          | x == '\n' = (acc, xs, True) -- Handle multiline strings if needed
          | otherwise = consumeString xs l (c + 1) (acc ++ [x])

        numberLiteral :: String -> Int -> Int -> [Token] -> [Token]
        numberLiteral s l c acc =
          let (intPart, afterInt) = span isDigit s
              (fracPart, rest) = case afterInt of
                                  ('.' : x : xs) | isDigit x -> 
                                    let (frac, remaining) = span isDigit (x : xs)
                                    in ("." ++ frac, remaining)
                                  _ -> ("", afterInt)
              numStr = intPart ++ fracPart
              num = read numStr :: Double
          in tokenize rest l (c + length numStr) (Token (Number num) numStr l : acc)

        identifier :: String -> Int -> Int -> [Token] -> [Token]
        identifier s l c acc =
          let (identStr, rest) = span (\x -> isAlpha x || isAlphaNum x) s
              tType = lookupKeyword identStr
          in tokenize rest l (c + length identStr) (Token tType identStr l : acc)

        lookupKeyword :: String -> TokenType
        lookupKeyword kw = case kw of
          "and"    -> And
          "class"  -> Class
          "else"   -> Else
          "false"  -> False_
          "fun"    -> Fun
          "for"    -> For
          "if"     -> If
          "nil"    -> Nil
          "or"     -> Or
          "print"  -> Print
          "return" -> Return
          "super"  -> Super
          "this"   -> This
          "true"   -> True_
          "var"    -> Var
          "while"  -> While
          _        -> Identifier kw
