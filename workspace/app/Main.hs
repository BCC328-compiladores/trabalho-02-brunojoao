module Main where

import System.Environment (getArgs)
import System.Exit (die)
import Data.Tree (drawTree)
import Prettyprinter
import Prettyprinter.Render.String (renderString)

import Lexer.Lexer
import Parser.Parser (parseProgram)
import AST.PrettyTree (programTree)
import AST.Pretty (prettyProgram)
import Token.Pretty (printTokens)
import Semantic.Analyzer (analyzeProgram)
import Errors.Pretty (renderDiagnostics)
import Interpreter.Eval (evalProgram)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--lexer", file]  -> runLexer file
    ["--parser", file] -> runParser file
    ["--pretty", file] -> runPretty file
    ["--semantic", file] -> runSemantic file
    ["--interp", file] -> runInterp file
    _                  -> die usage

usage :: String
usage = unlines
  [ "Usage:"
  , "  compiler --lexer <file>"
  , "  compiler --parser <file>"
  , "  compiler --pretty <file>"
  , "  compiler --semantic <file>"
  , "  compiler --interp <file>"
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

runSemantic :: FilePath -> IO ()
runSemantic file = do
  src <- readFile file
  let ast = parseProgram (alexScanTokens src)
  case analyzeProgram ast of
    Left ds -> die (renderDiagnostics ds)
    Right _ -> putStrLn "Semantic analysis: OK"

runInterp :: FilePath -> IO ()
runInterp file = do
  src <- readFile file
  let ast = parseProgram (alexScanTokens src)
  case analyzeProgram ast of
    Left ds -> die (renderDiagnostics ds)
    Right checked ->
      case evalProgram checked of
        Left err -> die ("Runtime error: " ++ err)
        Right outLines -> mapM_ putStrLn outLines