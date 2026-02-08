module TestSemantic where

import Test.HUnit
import Lexer.Lexer
import Parser.Parser
import Semantic.Analyzer (analyzeProgram)

assertSemanticOk :: String -> Assertion
assertSemanticOk src =
  case analyzeProgram (parseProgram (alexScanTokens src)) of
    Left ds -> assertFailure ("Expected semantic success, got: " ++ show ds)
    Right _ -> pure ()

assertSemanticErr :: String -> Assertion
assertSemanticErr src =
  case analyzeProgram (parseProgram (alexScanTokens src)) of
    Left _ -> pure ()
    Right _ -> assertFailure "Expected semantic error"

semanticTests :: Test
semanticTests = TestList
  [ "undeclared var" ~:
      TestCase (assertSemanticErr "func main(){ x = 1; }")
  , "type mismatch assign" ~:
      TestCase (assertSemanticErr "func main(){ let x:int = 1; x = true; }")
  , "function arity mismatch" ~:
      TestCase (assertSemanticErr "func f(a:int):int{ return a; } func main(){ f(); }")
  , "valid recursion" ~:
      TestCase (assertSemanticOk
        "func fact(n:int):int{ if(n<=1){return 1;} else {return n*fact(n-1);} } func main(){ let x:int = fact(5); print(x); }")
  , "array size property" ~:
      TestCase (assertSemanticOk
        "func main(){ let a:int[] = [1,2,3]; let n:int = a.size; print(n); }")
  , "forall generic identity" ~:
      TestCase (assertSemanticOk
        "forall a . func id(x:a):a { return x; } func main(){ let y:int = id(10); print(y); }")
  , "forall map function type param" ~:
      TestCase (assertSemanticOk
        "forall a b . func map(f:(a)->b, v:a[]) : b[] { let result = new b[v.size]; for(i=0; i<v.size; i++){ result[i] = f(v[i]); } return result; }")
  ]

main :: IO ()
main = do
  _ <- runTestTT semanticTests
  pure ()