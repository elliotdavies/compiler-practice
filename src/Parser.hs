module Parser where

import Lexer (Token(..))

import Data.Either.Combinators (mapLeft)
import Data.Functor.Alt ((<!>))

-- Types
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

maxDepth = 10

-- Helpers for parsing function definitions
parseName :: [Token] -> Parse String
parseName (Name n : ts) = Right (n, ts)
parseName ts = Left ("Couldn't parse fndef: name", ts)

parseParams :: [Token] -> Parse [Param]
parseParams (Name n : ts) =
  case parseParams ts of
    Right (ps, ts') -> Right (Param n : ps, ts')
    Left s -> Left s
parseParams (Equals : ts) = Right ([], ts)
parseParams ts = Left ("Couldn't parse fndef: params", ts)

parseBody :: [Token] -> Parse Expr
parseBody ts =
  case parseExpr ts maxDepth of
    Left (s, ts') -> Left ("Couldn't parse fndef: body. " ++ s, ts')
    Right r -> Right r

parseFnDef :: [Token] -> Parse FnDef
parseFnDef ts =
  mapLeft (\(s, _) -> (s, ts)) $ do
    (name, ts') <- parseName ts
    (params, ts'') <- parseParams ts'
    (body, ts''') <- parseBody ts''
    return (FnDef name params body, ts''')

-- Helpers for parsing expressions
parseExprs :: [Token] -> Int -> Parse [Expr]
parseExprs [] _ = Left ("No tokens to parse", [])
parseExprs ts depth =
  case parseExprsHelper ([], ts) depth of
    ([], _) -> Left ("No expressions parsed", ts)
    (es, ts') -> Right (es, ts')

parseExprsHelper :: ([Expr], [Token]) -> Int -> ([Expr], [Token])
parseExprsHelper (es, ts) depth =
  case parseExpr ts depth of
    Right (expr, ts') -> concatExpr expr $ parseExprs ts' depth
    Left _ -> (es, ts)

concatExpr :: Expr -> Parse [Expr] -> ([Expr], [Token])
concatExpr e (Left (s, ts)) = ([e], ts)
concatExpr e (Right (es, ts)) = (e:es, ts)

parseFnCall :: [Token] -> Int -> Parse Expr
parseFnCall ts depth =
  mapLeft (\(s, _) -> (s, ts)) $ do
    (Variable name, ts') <- parseVar ts
    (exprs, ts'') <- parseExprs ts' depth
    return (FnCall name exprs, ts'')

parseOperator :: [Token] -> Parse String
parseOperator (Operator o : ts) = Right (o, ts)
parseOperator ts = Left ("Couldn't parse infix operator", ts)

scanUntilOperator :: [Token] -> ([Token], [Token])
scanUntilOperator ts =
  scanHelper ([], ts)
  where
    scanHelper (xs, ys) =
      case ys of
        [] -> (xs, ys)
        (Operator _: ys') -> (xs, ys)
        (y: ys) -> scanHelper (xs ++ [y], ys)


parseInfix :: [Token] -> Int -> Parse Expr
parseInfix ts depth =
  let
    (tsBeforeOp, ts') = scanUntilOperator ts
  in
    mapLeft (\(s, _) -> (s, ts)) $ do
      (expr1, _) <- parseExpr tsBeforeOp depth
      (op, ts'') <- parseOperator ts'
      (expr2, ts''') <- parseExpr ts'' depth
      return (Infix op expr1 expr2, ts''')

parseVal :: [Token] -> Parse Expr
parseVal (Number n : ts) = Right (Value n, ts)
parseVal ts = Left ("Couldn't parse expr: val", ts)

parseVar :: [Token] -> Parse Expr
parseVar (Name n : ts) = Right (Variable n, ts)
parseVar ts = Left ("Couldn't parse expr: var", ts)

parseExpr :: [Token] -> Int -> Parse Expr
parseExpr ts depth =
  if depth == 0
  then Left ("Max depth exceeded", ts)
  else
    -- First try to parse a function call
    parseFnCall ts (depth - 1) <!>
    -- Else try to parse an infix operator
    parseInfix ts (depth - 1) <!>
    -- Else a value
    parseVal ts <!>
    -- Else a variable
    parseVar ts

parseDecl :: [Token] -> Parse Decl
parseDecl (LineBreak : ts) = parseDecl ts
parseDecl ts =
  -- First try to parse a FnDef
  case parseFnDef ts of
    Right (fnDef, ts') -> Right (FnDecl fnDef, ts')
    Left s ->
      -- If unsuccessful, try to parse an Expr
      case parseExpr ts maxDepth of
        Right (expr, ts') -> Right (EDecl expr, ts')
        -- If still unsuccessful, return an error
        Left s -> Left s

parseTokens :: [Token] -> Either Err [Decl]
parseTokens ts =
  case parseDecl ts of
    Right (decl, ts') -> concatDecl decl $ parseTokens ts'
    Left s -> Left s

concatDecl :: Decl -> Either Err [Decl] -> Either Err [Decl]
concatDecl d (Left s) = Right [d]
concatDecl d (Right ds) = Right (d:ds)


-- Parser
parser :: [Token] -> Either Err AST
parser ts =
  case parseTokens ts of
    Right ts -> Right (AST ts)
    Left s -> Left s
