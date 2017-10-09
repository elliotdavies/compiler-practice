module Main where

import Lexer (lexer, Token)
import Parser (parser, AST(AST))
import CodeGen (generateCode)
import Helpers (join)

main :: IO ()
main = do
  sourceCode <- readFile "samples/test2.elliot"
  let tokens = lexer sourceCode
  putStrLn $ "\nTokens (" ++ show (length tokens) ++ "):"
  putStrLn $ printTokens tokens
  let ast = parser tokens
  putStrLn "\nAST:"
  putStrLn $ printAst ast
  printCodeGen ast

  where
    printCodeGen ast =
      case ast of
        Right a -> do
          putStrLn "\nCode:"
          putStrLn $ generateCode a

        Left a -> return ()

-- Pretty printing
printTokens :: [Token] -> String
printTokens = join ", " . map show

printAst :: Either (String, [Token]) AST -> String
printAst (Right (AST defs)) = join "\n" $ map show defs
printAst (Left (s, ts)) = s ++ "\n" ++ (printTokens ts)
