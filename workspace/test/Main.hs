module Main where

import TestLexer
import TestParser
import qualified TestSemantic
import qualified TestInterpreter
import qualified TestLimitations
import Test.HUnit (runTestTT, Counts(..))
import System.Exit (exitFailure)

main :: IO ()
main = do
  putStrLn "Executando testes..."
  c1 <- runTestTT lexerTests
  c2 <- runTestTT parserTests
  c3 <- runTestTT TestSemantic.semanticTests
  c4 <- runTestTT TestInterpreter.interpreterTests
  c5 <- runTestTT TestLimitations.limitationsTests
  if hasFailure [c1, c2, c3, c4, c5]
    then exitFailure
    else pure ()

hasFailure :: [Counts] -> Bool
hasFailure = any (\c -> errors c > 0 || failures c > 0)