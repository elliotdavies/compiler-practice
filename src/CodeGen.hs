module CodeGen where

import Helpers (join)
import Parser (AST(..), Decl(..), FnDef(..), Param(..), Expr(..))

generateCode :: AST -> String
generateCode (AST decls) =
  join "\n" $ fmap generateDecl decls

generateDecl :: Decl -> String
generateDecl decl =
  case decl of
    FnDecl fnDef ->
      generateFnDef fnDef

    EDecl eDef ->
      "edecls not yet implemented"

generateFnDef :: FnDef -> String
generateFnDef (FnDef name params body) =
  let
    generateParam (Param p) = p
    generateParams ps = join ", " $ fmap generateParam ps
  in
    "const " ++ name ++ " = (" ++
    (generateParams params) ++ ") => " ++
    (generateExpr body) ++ ";"

generateExpr :: Expr -> String
generateExpr expr =
  let
    generateExprs exprs = join ", " $ fmap generateExpr exprs
  in
    case expr of
      FnCall name exprs ->
        name ++ "(" ++ generateExprs exprs ++ ")"

      Infix op expr1 expr2 ->
        generateExpr expr1 ++ " " ++ op ++ " " ++ generateExpr expr2

      Value n ->
        show n

      Variable s ->
        s
