module CodeGen where

import Helpers (join)
import Parser (AST(..), Decl(..), FnDef(..), Param(..), Expr(..))


-- Generate code from an AST
generateCode :: AST -> String
generateCode (AST decls) =
  join "\n" $ fmap generateDecl decls

-- Generate code for a declaration
generateDecl :: Decl -> String
generateDecl decl =
  case decl of
    FnDecl fnDef ->
      generateFnDef fnDef

    EDecl eDef ->
      generateEDecl eDef

-- Generate code for a function definition
generateFnDef :: FnDef -> String
generateFnDef (FnDef name params body) =
  let
    generateParam (Param p) =
      p

    generateParams ps =
      join ", " $ fmap generateParam ps
  in
    "const " ++ name ++ " = (" ++
    (generateParams params) ++ ") => " ++
    (generateExpr body) ++ ";"

-- Generate code for a top-level expression
generateEDecl :: Expr -> String
generateEDecl expr =
  let
    edecl =
      case expr of
        Variable name ->
          generateFnCall name []

        otherwise ->
          generateExpr expr
  in
    edecl ++ ";"

-- Generate code for an expression
generateExpr :: Expr -> String
generateExpr expr =
  case expr of
    FnCall name exprs ->
      generateFnCall name exprs

    Infix op expr1 expr2 ->
      generateExpr expr1 ++ " " ++ op ++ " " ++ generateExpr expr2

    Value n ->
      show n

    Variable s ->
      s

generateFnCall :: String -> [Expr] -> String
generateFnCall name exprs =
  let
    generateExprs exprs =
      join ", " $ fmap generateExpr exprs
  in
    name ++ "(" ++ generateExprs exprs ++ ")"
