module TestLimitations where

import Test.HUnit
import Lexer.Lexer
import Parser.Parser
import Semantic.Analyzer (analyzeProgram)
import Interpreter.Eval (evalProgram)
import Errors.Diagnostic (Diagnostic(..))
import Data.List (isInfixOf)

assertSemanticErrorCode :: String -> String -> Assertion
assertSemanticErrorCode expectedCode src =
  case analyzeProgram (parseProgram (alexScanTokens src)) of
    Left ds ->
      if any (\d -> diagCode d == expectedCode) ds
        then pure ()
        else assertFailure ("Expected semantic code " ++ expectedCode ++ ", got: " ++ show ds)
    Right _ ->
      assertFailure ("Expected semantic error " ++ expectedCode ++ ", got success")

assertRuntimeErrorContains :: String -> String -> Assertion
assertRuntimeErrorContains expectedMsg src =
  case analyzeProgram (parseProgram (alexScanTokens src)) of
    Left ds -> assertFailure ("Expected semantic success, got: " ++ show ds)
    Right checked ->
      case evalProgram checked of
        Left err ->
          if expectedMsg `isInfixOf` err
            then pure ()
            else assertFailure ("Expected runtime error containing '" ++ expectedMsg ++ "', got: " ++ err)
        Right out ->
          assertFailure ("Expected runtime failure, got output: " ++ show out)

limitationsTests :: Test
limitationsTests = TestList
  [ "limitation:first-class function value in call site" ~:
      TestCase (assertSemanticErrorCode "SEM_VAR_UNDECL"
        (unlines
          [ "forall a b . func apply(f:(a)->b, x:a):b { return f(x); }"
          , "func inc(x:int):int { return x + 1; }"
          , "func main(){ let y:int = apply(inc, 10); print(y); }"
          ]))
  , "runtime:error array bounds" ~:
      TestCase (assertRuntimeErrorContains "Array index out of bounds"
        "func main(){ let a = [1,2,3]; print(a[10]); }")
  ]

main :: IO ()
main = do
  _ <- runTestTT limitationsTests
  pure ()