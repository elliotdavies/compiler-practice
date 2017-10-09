module Main where

import Data.List (intersperse)

import Lexer (lexer, Token)
import Parser (parser, AST(AST))

main :: IO ()
main = do
  sourceCode <- readFile "samples/test2.elliot"
  let tokens = lexer sourceCode
  putStrLn $ "\nTokens (" ++ show (length tokens) ++ "):"
  putStrLn $ printTokens tokens
  let ast = parser tokens
  putStrLn "\nAST:"
  putStrLn $ printAst ast

-- Pretty printing
join :: String -> [String] -> String
join s = foldr (++) "" . intersperse s

printTokens :: [Token] -> String
printTokens = join ", " . map show

printAst :: Either (String, [Token]) AST -> String
printAst (Right (AST defs)) = join "\n" $ map show defs
printAst (Left (s, ts)) = s ++ "\n" ++ (printTokens ts)
