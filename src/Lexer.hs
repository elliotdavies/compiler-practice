module Lexer where

import Data.Char (isDigit)
import Data.List (intersperse)


{- Types
-}

data Token
  = Equals
  | Operator String
  | Name String
  | Number Int
  | LineBreak
  deriving (Show, Eq)


{- Helpers
-}

toToken :: String -> Token
toToken s
  | s == "=" = Equals
  | s == "+" = Operator s
  | all isDigit s = Number $ read s
  | otherwise = Name s


{- The lexer function
-}

lexer :: String -> [Token]
lexer =
  let
    lineToTokens =
      map toToken . words
  in
    concat .
    intersperse [LineBreak] .
    map lineToTokens .
    filter (/= "") .
    lines
