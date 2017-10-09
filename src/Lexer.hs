module Lexer where

import Data.Char (isDigit)

-- Types
data Token
  = Equals
  | Operator String
  | Name String
  | Number Int
  | LineBreak
  deriving (Show, Eq)

-- Helpers
toToken :: String -> Token
toToken s
  | s == "=" = Equals
  | s == "+" = Operator s
  | all isDigit s = Number $ read s
  | otherwise = Name s

-- Lexer
lexer :: String -> [Token]
lexer =
  let
    lineToTokens = map toToken . words
    withBreak s = lineToTokens s ++ [LineBreak]
    notEmpty s = s /= ""
  in
    concatMap withBreak . filter notEmpty . lines
