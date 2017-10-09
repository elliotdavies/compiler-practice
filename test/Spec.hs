module Main where

import Test.HUnit
import Lexer (lexer, Token(..))
import Parser

main :: IO ()
main = do
  runTestTT lexerTests
  runTestTT parserTests
  return ()

-- Test the lexer
lexerTests = TestLabel "Lexer" $ test [

  "lexes simple assignments" ~: do
    let src = "y = 1"
    let expected = [ Name "y", Equals, Number 1 ]
    assertEqual "1" expected (lexer src)
  ,

  "lexes simple function defs" ~: do
    let src = "f x = 1"
    let expected = [ Name "f", Name "x", Equals, Number 1 ]
    assertEqual "1" expected (lexer src)
  ,

  "lexes function defs with operators" ~: do
    let src = "f a b = a + b"
    let expected = [ Name "f", Name "a", Name "b", Equals, Name "a", Operator "+", Name "b" ]
    assertEqual "1" expected (lexer src)

  ]

-- Test the parser
parserTests = TestLabel "Parser" $ test [

  -- Function defs
  TestLabel "FnDefs" $ test [

    "parses defs" ~: do
      let tks = [ Name "f", Name "x", Name "y", Equals, Number 1 ]
      let expectedDef = FnDef "f" [Param "x", Param "y"] (Value 1)
      let expectedParse = Right (expectedDef, [])
      assertEqual "1" expectedParse (parseFnDef tks)

    ],

  -- Exprs
  TestLabel "Exprs" $ test [

    "parseVar" ~: do
      let tks = [ Name "x" ]
      let expectedDef = Variable "x"
      let expectedParse = Right (expectedDef, [])
      assertEqual "parses vars" expectedParse (parseVar tks)

      let tks = [ Name "x", Name "y" ]
      let expectedDef = Variable "x"
      let expectedParse = Right (expectedDef, [ Name "y" ])
      assertEqual "returns other tokens" expectedParse (parseVar tks)

      let tks = [ Number 1 ]
      let expectedParse = Left ("Couldn't parse expr: var", tks)
      assertEqual "should fail for non-vars" expectedParse (parseVar tks)
    ,

    "parseVal" ~: do
      let tks = [ Number 1 ]
      let expectedDef = Value 1
      let expectedParse = Right (expectedDef, [])
      assertEqual "parses vals" expectedParse (parseVal tks)

      let tks = [ Number 1, Name "a" ]
      let expectedDef = Value 1
      let expectedParse = Right (expectedDef, [ Name "a" ])
      assertEqual "returns other tokens" expectedParse (parseVal tks)

      let tks = [ Name "a" ]
      let expectedParse = Left ("Couldn't parse expr: val", tks)
      assertEqual "should fail for non-vals" expectedParse (parseVal tks)
    ,

    "parseInfix" ~: do
      let tks = [ Number 1, Operator "+", Number 2 ]
      let expectedDef = Infix "+" (Value 1) (Value 2)
      let expectedParse = Right (expectedDef, [])
      assertEqual "parses infixes" expectedParse (parseInfix tks 1)

      let tks = [ Number 1, Operator "+", Number 2 ]
      let expectedDef = Infix "+" (Value 1) (Value 2)
      let expectedParse = Right (expectedDef, [])
      assertEqual "at arbitrary depth" expectedParse (parseInfix tks 3)

      let tks = [ Number 1, Operator "+", Number 2, Number 3 ]
      let expectedDef = Infix "+" (Value 1) (Value 2)
      let expectedParse = Right (expectedDef, [ Number 3 ])
      assertEqual "returns other tokens" expectedParse (parseInfix tks 2)

      let tks = [ Name "a", Operator "+", Number 2, Number 3 ]
      let expectedDef = Infix "+" (Variable "a") (Value 2)
      let expectedParse = Right (expectedDef, [ Number 3 ])
      assertEqual "with mixed exprs" expectedParse (parseInfix tks 2)

      let tks = [ Operator "+", Number 2 ]
      let expectedParse = Left ("Couldn't parse expr: var", tks)
      assertEqual "should fail if no first expr is found" expectedParse (parseInfix tks 2)

      let tks = [ Name "a", Number 2 ]
      let expectedParse = Left ("Couldn't parse infix operator", tks)
      assertEqual "should fail if no operator is found" expectedParse (parseInfix tks 2)

      let tks = [ Name "a", Operator "+" ]
      let expectedParse = Left ("Couldn't parse expr: var", tks)
      assertEqual "should fail if no second expr is found" expectedParse (parseInfix tks 2)
    ,

    "parseFnCall" ~: do
      let tks = [ Name "f", Number 1, Number 2 ]
      let expectedDef = FnCall "f" [ Value 1, Value 2 ]
      let expectedParse = Right (expectedDef, [])
      assertEqual "parses fn calls" expectedParse (parseFnCall tks 1)

      let tks = [ Number 1, Name "f", Number 2 ]
      let expectedParse = Left ("Couldn't parse expr: var", tks)
      assertEqual "should fail for invalid calls" expectedParse (parseFnCall tks 1)
    ,

    "parseExpr" ~: do
      let tks = [ Name "a" ]
      let expectedDef = Variable "a"
      let expectedParse = Right (expectedDef, [])
      assertEqual "var" expectedParse (parseExpr tks 2)

      let tks = [ Number 1 ]
      let expectedDef = Value 1
      let expectedParse = Right (expectedDef, [])
      assertEqual "val" expectedParse (parseExpr tks 2)

      let tks = [ Number 1, Operator "+", Name "b" ]
      let expectedDef = Infix "+" (Value 1) (Variable "b")
      let expectedParse = Right (expectedDef, [])
      assertEqual "infix" expectedParse (parseExpr tks 2)
    ,

    "parseExprs" ~: do
      let tks = [ Name "a", Number 2, Number 3 ]
      let expectedDefs = [ Variable "a", Value 2, Value 3 ]
      let expectedParse = Right (expectedDefs, [])
      assertEqual "parses multiple exprs" expectedParse (parseExprs tks 1)

      let tks = [ Number 2, Operator "+", Number 3 ]
      let expectedDefs = [ Infix "+" (Value 2) (Value 3) ]
      let expectedParse = Right (expectedDefs, [])
      assertEqual "parses infixes" expectedParse (parseExprs tks 2)

      let tks = [ Number 2, Operator "+", Number 3, Name "a" ]
      let expectedDefs = [ Infix "+" (Value 2) (Value 3), Variable "a" ]
      let expectedParse = Right (expectedDefs, [])
      assertEqual "parses multiple exprs incl infixes" expectedParse (parseExprs tks 2)
    ,

    "scanUntilOperator" ~: do
      let tks = [ Number 1, Name "a", Operator "+", Number 2 ]
      let expected = ([ Number 1, Name "a" ], [ Operator "+", Number 2 ])
      assertEqual "scans until an operator" expected (scanUntilOperator tks)
    ,

    "concatExpr" ~: do
      let expr = Value 1
      let parses = Right ([ Value 2 ], [])
      let expected = ([ Value 1, Value 2 ], [])
      assertEqual "adds an expr to a list of parses" expected (concatExpr expr parses)

    ]

  ]
