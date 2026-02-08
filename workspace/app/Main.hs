module Main where

import System.Environment (getArgs)
import System.Exit (die)
import Data.Tree (drawTree)
import Prettyprinter
import Prettyprinter.Render.String (renderString)

import Lexer
import Parser (parseProgram)
import ASTPretty (programTree)
import CodePretty (prettyProgram)
import TokenPretty (printTokens)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--lexer", file]  -> runLexer file
    ["--parser", file] -> runParser file
    ["--pretty", file] -> runPretty file
    _                  -> die usage

usage :: String
usage = unlines
  [ "Usage:"
  , "  compiler --lexer <file>"
  , "  compiler --parser <file>"
  , "  compiler --pretty <file>"
  ]

runLexer :: FilePath -> IO ()
runLexer file = do
  src <- readFile file
  printTokens (alexScanTokens src)

runParser :: FilePath -> IO ()
runParser file = do
  src <- readFile file
  let ast = parseProgram (alexScanTokens src)
  putStrLn (drawTree (programTree ast))

runPretty :: FilePath -> IO ()
runPretty file = do
  src <- readFile file
  let ast = parseProgram (alexScanTokens src)
  putStrLn (renderString (layoutPretty defaultLayoutOptions (prettyProgram ast)))

