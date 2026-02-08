module TestLexer where

import Test.HUnit
import Lexer
import Token
import Control.Exception (SomeException, try, evaluate)

tag :: Token -> String
tag t = case t of
  TypeInt{}      -> "TypeInt"
  TypeFloat{}    -> "TypeFloat"
  TypeBool{}     -> "TypeBool"
  TypeString{}   -> "TypeString"
  TypeVoid{}     -> "TypeVoid"
  Identifier{}   -> "Identifier"
  ValueInt{}     -> "ValueInt"
  ValueFloat{}   -> "ValueFloat"
  ValueString{}  -> "ValueString"
  ValueBool{}    -> "ValueBool"
  Let{}          -> "Let"
  Return{}       -> "Return"
  If{}           -> "If"
  Else{}         -> "Else"
  While{}        -> "While"
  For{}          -> "For"
  Func{}         -> "Func"
  Struct{}       -> "Struct"
  Forall{}       -> "Forall"
  New{}          -> "New"
  Plus{}         -> "Plus"
  Minus{}        -> "Minus"
  Multiply{}     -> "Multiply"
  Divide{}       -> "Divide"
  Assign{}       -> "Assign"
  Equal{}        -> "Equal"
  NotEqual{}     -> "NotEqual"
  Less{}         -> "Less"
  Greater{}      -> "Greater"
  LessEqual{}    -> "LessEqual"
  GreaterEqual{} -> "GreaterEqual"
  And{}          -> "And"
  Or{}           -> "Or"
  Arrow{}        -> "Arrow"
  Inc{}          -> "Inc"
  Comma{}        -> "Comma"
  Semicolon{}    -> "Semicolon"
  Dot{}          -> "Dot"
  LParenthesis{} -> "LParenthesis"
  RParenthesis{} -> "RParenthesis"
  LBrace{}       -> "LBrace"
  RBrace{}       -> "RBrace"
  LBracket{}     -> "LBracket"
  RBracket{}     -> "RBracket"
  TypeDef{}      -> "TypeDef"

lexTags :: String -> [String]
lexTags = map tag . alexScanTokens

assertThrows :: IO a -> Assertion
assertThrows action = do
  r <- try (action >> return ()) :: IO (Either SomeException ())
  case r of
    Left _  -> return ()
    Right _ -> assertFailure "Expected exception"

lexerTests :: Test
lexerTests = TestList
  [ "int var" ~:
      lexTags "int x;" ~?=
        ["TypeInt","Identifier","Semicolon"]

  , "let init" ~:
      lexTags "let x = 42;" ~?=
        ["Let","Identifier","Assign","ValueInt","Semicolon"]

  , "float literal" ~:
      lexTags "let y = 3.14;" ~?=
        ["Let","Identifier","Assign","ValueFloat","Semicolon"]

  , "bool true false" ~:
      lexTags "true false" ~?=
        ["ValueBool","ValueBool"]

  , "string empty" ~:
      lexTags "\"\"" ~?=
        ["ValueString"]

  , "identifier vs keyword" ~:
      lexTags "intx int" ~?=
        ["Identifier","TypeInt"]

  , "operators precedence" ~:
      lexTags "a+b*c" ~?=
        ["Identifier","Plus","Identifier","Multiply","Identifier"]

  , "logical ops" ~:
      lexTags "a&&b||c" ~?=
        ["Identifier","And","Identifier","Or","Identifier"]

  , "array syntax" ~:
      lexTags "a[10]" ~?=
        ["Identifier","LBracket","ValueInt","RBracket"]

  , "struct access" ~:
      lexTags "a.b.c" ~?=
        ["Identifier","Dot","Identifier","Dot","Identifier"]

  , "new array" ~:
      lexTags "new int[5]" ~?=
        ["New","TypeInt","LBracket","ValueInt","RBracket"]

  , "comment only" ~:
      lexTags "// hello" ~?=
        []

  , "mixed whitespace" ~:
      lexTags " let   x\t=\n1 ; " ~?=
        ["Let","Identifier","Assign","ValueInt","Semicolon"]

  , "lexical error" ~:
      TestCase $
        assertThrows (evaluate (head (alexScanTokens "@")))
  ]

main :: IO ()
main = do
  _ <- runTestTT lexerTests
  return ()
