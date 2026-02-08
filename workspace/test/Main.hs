module Main where

import TestLexer
import TestParser

main :: IO ()
main = do
  putStrLn "Executando testes..."
  TestLexer.main
  TestParser.main