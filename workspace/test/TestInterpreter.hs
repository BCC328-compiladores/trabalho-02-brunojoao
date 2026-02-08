module TestInterpreter where

import Test.HUnit
import Lexer.Lexer
import Parser.Parser
import Semantic.Analyzer (analyzeProgram)
import Interpreter.Eval (evalProgram)

runInterp :: String -> Either String [String]
runInterp src = do
  checked <- case analyzeProgram (parseProgram (alexScanTokens src)) of
    Left ds -> Left ("Semantic error: " ++ show ds)
    Right ok -> Right ok
  evalProgram checked

interpreterTests :: Test
interpreterTests = TestList
  [ "print arithmetic" ~:
      TestCase $
        case runInterp "func main(){ print(1+2*3); }" of
          Right out -> out @?= ["7"]
          Left e -> assertFailure e
  , "while increment" ~:
      TestCase $
        case runInterp "func main(){ let i:int = 0; while(i < 3){ print(i); i = i + 1; } }" of
          Right out -> out @?= ["0", "1", "2"]
          Left e -> assertFailure e
  , "struct field" ~:
      TestCase $
        case runInterp "struct S { x:int; } func main(){ let s = S{10}; print(s.x); }" of
          Right out -> out @?= ["10"]
          Left e -> assertFailure e
  , "for update ++" ~:
      TestCase $
        case runInterp "func main(){ let i:int = 0; for(i=0; i<3; i++){ print(i); } }" of
          Right out -> out @?= ["0", "1", "2"]
          Left e -> assertFailure e
  , "array size runtime" ~:
      TestCase $
        case runInterp "func main(){ let a = [1,2,3,4]; print(a.size); }" of
          Right out -> out @?= ["4"]
          Left e -> assertFailure e
  , "generic id runtime" ~:
      TestCase $
        case runInterp "forall a . func id(x:a):a { return x; } func main(){ print(id(42)); }" of
          Right out -> out @?= ["42"]
          Left e -> assertFailure e
  ]

main :: IO ()
main = do
  _ <- runTestTT interpreterTests
  pure ()