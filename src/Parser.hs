module Parser where

import Lexer (Token(..))
import Debug.Trace (trace)

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
    Right (expr, ts') -> Right (expr, ts')

parseFnDef :: [Token] -> Parse FnDef
parseFnDef ts = do
  (name, ts') <- parseName ts
  (params, ts'') <- parseParams ts'
  (body, ts''') <- parseBody ts''
  return (FnDef name params body, ts''')

-- Helpers for parsing expressions
parseExprs :: [Token] -> Int -> Parse [Expr]
parseExprs ts depth =
  case parseExpr ts depth of
    Right (expr, ts') -> concatExpr expr $ parseExprs ts' depth
    Left s -> Left s

concatExpr :: Expr -> Parse [Expr] -> Parse [Expr]
concatExpr _ (Left s) = Left s
concatExpr expr (Right (exprs, ts)) = Right ((expr : exprs), ts)

parseFnCall :: [Token] -> Int -> Parse Expr
parseFnCall ts depth = do
  (Variable name, ts') <- parseVar ts
  (exprs, ts'') <- parseExprs ts' depth
  return (FnCall name exprs, ts'')

parseOperator :: [Token] -> Parse String
parseOperator (Operator o : ts) = Right (o, ts)
parseOperator ts = Left ("Couldn't parse infix operator", ts)

parseInfix :: [Token] -> Int -> Parse Expr
parseInfix ts depth = do
  (expr1, ts') <- parseExpr ts depth
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
    -- case parseFnCall ts (depth - 1) of
    --   Right (fnCall, ts') -> Right (fnCall, ts')
    --   Left s ->
        -- Else try to parse an infix operator
        case parseInfix ts (depth - 1) of
          Right (infix', ts') -> Right (infix', ts')
          Left s ->
            -- Else a value
            case parseVal ts of
              Right (val, ts') -> Right (val, ts')
              Left s ->
                -- Else a variable
                case parseVar ts of
                  Right (var, ts') -> Right (var, ts')
                  Left s -> Left s

-- Return an error if unable to parse a decl, or
-- return the decl and any remaining tokens
maxDepth = 10

-- @TODO How best to chain these?
parseDecl :: [Token] -> Parse Decl
parseDecl ts =
  -- First try to parse a FnDef
  case parseFnDef ts of
    Right (fnDef, ts') -> Right (FnDecl fnDef, ts')
    Left s ->
      -- If unsuccessful, try to parse am Expr
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
concatDecl _ (Left s) = Left s
concatDecl decl (Right decls) = Right (decl : decls)

-- Parser
parser :: [Token] -> Either Err AST
parser ts =
  case parseTokens ts of
    Right ts -> Right (AST ts)
    Left s -> Left s