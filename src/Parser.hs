module Parser where

import Lexer (Token(..))

import Data.Either.Combinators (mapLeft, mapRight)
import Data.Functor.Alt ((<!>))
import Control.Arrow (first)


{- Types
-}
data AST
  = AST [ Decl ]
  deriving (Show, Eq)

data Decl
  = FnDecl FnDef
  | EDecl Expr
  deriving (Show, Eq)

data FnDef
  = FnDef String [Param] Expr
  deriving (Show, Eq)

data Param = Param String
  deriving (Show, Eq)

data Expr
  = FnCall String [Expr]
  | Infix String Expr Expr
  | Value Int
  | Variable String
  deriving (Show, Eq)

type Err = (String, [Token])
type Parse a = Either Err (a, [Token])


{- Settings
-}

maxDepth = 10


{- Helpers for parsing function definitions
-}

-- Parse a function name
parseName :: [Token] -> Parse String
parseName tokens =
  case tokens of
    (Name n : ts) ->
      Right (n, ts)

    otherwise ->
      Left ("Couldn't parse fndef: name", tokens)

-- Parse function params (`Name`s until the first `Equals`)
parseParams :: [Token] -> Parse [Param]
parseParams tokens =
  case tokens of
    (Name n : ts) ->
      case parseParams ts of
        Right (ps, ts') ->
          Right (Param n : ps, ts')

        Left s ->
          Left s

    (Equals : ts) ->
      Right ([], ts)

    otherwise ->
      Left ("Couldn't parse fndef: params", tokens)

-- Parse a function body expression
parseBody :: [Token] -> Parse Expr
parseBody tokens =
  case parseExpr tokens maxDepth of
    Left (s, ts) ->
      Left ("Couldn't parse fndef: body. " ++ s, ts)

    Right r ->
      Right r

-- Parse an overall function definition
parseFnDef :: [Token] -> Parse FnDef
parseFnDef tokens =
  mapLeft (\(s, _) -> (s, tokens)) $ do
    (name, ts) <- parseName tokens
    (params, ts') <- parseParams ts
    (body, ts'') <- parseBody ts'
    return (FnDef name params body, ts'')


{- Helpers for parsing expressions
-}

-- Parse multiple expressions
parseExprs :: [Token] -> Int -> Parse [Expr]
parseExprs tokens depth =
  case tokens of
    [] ->
      Left ("No tokens to parse", [])

    otherwise ->
      case parseExprsHelper ([], tokens) depth of
        ([], _) ->
          Left ("No expressions parsed", tokens)

        (es, ts) ->
          Right (es, ts)

-- Helper
parseExprsHelper :: ([Expr], [Token]) -> Int -> ([Expr], [Token])
parseExprsHelper (es, ts) depth =
  case parseExpr ts depth of
    Right (expr, ts') ->
      concatExpr expr $ parseExprs ts' depth

    Left _ ->
      (es, ts)

-- Concatenate expressions
concatExpr :: Expr -> Parse [Expr] -> ([Expr], [Token])
concatExpr e parse =
  case parse of
    Left (s, ts) ->
      ([e], ts)

    Right (es, ts) ->
      (e:es, ts)

-- Parse a function call
parseFnCall :: [Token] -> Int -> Parse Expr
parseFnCall ts depth =
  mapLeft (\(s, _) -> (s, ts)) $ do
    (Variable name, ts') <- parseVar ts
    (exprs, ts'') <- parseExprs ts' depth
    return (FnCall name exprs, ts'')

-- Parse an infix operator
parseOperator :: [Token] -> Parse String
parseOperator tokens =
  case tokens of
    (Operator o : ts) ->
      Right (o, ts)

    otherwise ->
      Left ("Couldn't parse infix operator", tokens)

-- Scan ahead in the token stream until an operator is found
scanUntilOperator :: [Token] -> ([Token], [Token])
scanUntilOperator tokens =
  scanHelper ([], tokens)
  where
    scanHelper (xs, ys) =
      case ys of
        [] ->
          (xs, ys)

        (Operator _: ys') ->
          (xs, ys)

        (y: ys') ->
          scanHelper (xs ++ [y], ys')

-- Parse an infix expression
parseInfix :: [Token] -> Int -> Parse Expr
parseInfix ts depth =
  let
    (tsBeforeOp, ts') =
        scanUntilOperator ts
  in
    mapLeft (\(s, _) -> (s, ts)) $ do
      (expr1, _) <- parseExpr tsBeforeOp depth
      (op, ts'') <- parseOperator ts'
      (expr2, ts''') <- parseExpr ts'' depth
      return (Infix op expr1 expr2, ts''')

-- Parse a value
parseVal :: [Token] -> Parse Expr
parseVal tokens =
  case tokens of
    (Number n : ts) ->
      Right (Value n, ts)

    otherwise ->
      Left ("Couldn't parse expr: val", tokens)

-- Parse a variable
parseVar :: [Token] -> Parse Expr
parseVar tokens =
  case tokens of
    (Name n : ts) ->
      Right (Variable n, ts)

    otherwise ->
      Left ("Couldn't parse expr: var", tokens)

-- Parse an expression
parseExpr :: [Token] -> Int -> Parse Expr
parseExpr tokens depth =
  if depth == 0
  then Left ("Max depth exceeded", tokens)
  else
    -- First try to parse a function call
    parseFnCall tokens (depth - 1) <!>
    -- Else try to parse an infix operator
    parseInfix tokens (depth - 1) <!>
    -- Else try a value
    parseVal tokens <!>
    -- Else try a variable
    parseVar tokens


{- Helpers for parsing a whole token stream
-}

-- Parse a declaration
parseDecl :: [Token] -> Parse Decl
parseDecl tokens =
  case tokens of
    (LineBreak : ts) ->
      parseDecl ts

    otherwise ->
      -- First try to parse a `FnDef` into a `FnDecl`
      (mapRight (first FnDecl) $ parseFnDef tokens) <!>
      -- Else try to parse an `Expr` into an `EDecl`
      (mapRight (first EDecl) $ parseExpr tokens maxDepth)

-- Concatenate declarations
concatDecl :: Decl -> Either Err [Decl] -> Either Err [Decl]
concatDecl d res =
  case res of
    Left s ->
      Right [d]

    Right ds ->
      Right (d:ds)

-- Parse a token stream
parseTokens :: [Token] -> Either Err [Decl]
parseTokens ts =
  case parseDecl ts of
    Right (decl, ts') ->
      concatDecl decl $ parseTokens ts'

    Left s ->
      Left s

{- The parser function
-}
parser :: [Token] -> Either Err AST
parser tokens =
  mapRight AST $ parseTokens tokens
