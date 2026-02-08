module TestParser where

import Test.HUnit
import Lexer
import Parser
import Control.Exception (SomeException, try, evaluate)

parseOk :: String -> IO ()
parseOk src = evaluate (parseProgram (alexScanTokens src)) >> return ()

assertThrows :: IO a -> Assertion
assertThrows action = do
  r <- try (action >> return ()) :: IO (Either SomeException ())
  case r of
    Left _  -> return ()
    Right _ -> assertFailure "Expected parser error"

parserTests :: Test
parserTests = TestList
  [ "var typed" ~:
      TestCase (parseOk "let x:int = 1;")

  , "var untyped" ~:
      TestCase (parseOk "let x = 1;")

  , "function no params" ~:
      TestCase (parseOk
        "func f() { return; }")

  , "function typed return" ~:
      TestCase (parseOk
        "func f():int { return 1; }")

  , "if else" ~:
      TestCase (parseOk
        "func f(){ if(true){return;} else {return;} }")

  , "while" ~:
      TestCase (parseOk
        "func f(){ while(true){return;} }")

  , "for full" ~:
      TestCase (parseOk
        "func f(){ for(i=0; i<10; i++){ return; } }")

  , "nested lvalue" ~:
      TestCase (parseOk
        "func f(){ a.b[0].c = 1; }")

  , "call and expr" ~:
      TestCase (parseOk
        "func f(){ g(1,2+3*4); }")

  , "array literal" ~:
      TestCase (parseOk
        "func f(){ let a = [1,2,3]; }")

  , "struct decl" ~:
      TestCase (parseOk
        "struct S { x:int; y:int; }")

  , "struct init" ~:
      TestCase (parseOk
        "func f(){ let s = S{1,2}; }")

  , "new expr" ~:
      TestCase (parseOk
        "func f(){ let a = new int[10]; }")

  , "precedence" ~:
      TestCase (parseOk
        "func f(){ let x = 1 + 2 * 3 < 10 && true; }")

  , "syntax error" ~:
      TestCase $
        assertThrows (parseOk "func f( { }")
  ]

main :: IO ()
main = do
  _ <- runTestTT parserTests
  return ()
