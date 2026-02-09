module Main where

import System.Environment (getArgs)
import System.Exit (die)
import Data.Tree (drawTree)
import Data.List (isInfixOf, stripPrefix)
import Prettyprinter
import Prettyprinter.Render.String (renderString)

import Lexer.Lexer
import Parser.Parser (parseProgram)
import AST.PrettyTree (programTree)
import AST.Pretty (prettyProgram)
import Token.Pretty (printTokens)
import Token.Token (Token(..), Position(..))
import Semantic.Analyzer (analyzeProgram)
import Errors.Pretty (renderDiagnostics)
import Errors.Diagnostic (Diagnostic(..))
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
  let toks = alexScanTokens src
  let ast = parseProgram toks
  case analyzeProgram ast of
    Left ds -> die (renderDiagnostics (attachSemanticPositions toks ds))
    Right _ -> putStrLn "Semantic analysis: OK"

runInterp :: FilePath -> IO ()
runInterp file = do
  src <- readFile file
  let toks = alexScanTokens src
  let ast = parseProgram toks
  case analyzeProgram ast of
    Left ds -> die (renderDiagnostics (attachSemanticPositions toks ds))
    Right checked ->
      case evalProgram checked of
        Left err -> die ("Runtime error: " ++ attachRuntimePosition toks err)
        Right outLines -> mapM_ putStrLn outLines

attachSemanticPositions :: [Token] -> [Diagnostic] -> [Diagnostic]
attachSemanticPositions toks = map (attachOne toks)
  where
    attachOne _ d@(Diagnostic { diagPos = Just _ }) = d
    attachOne ts d =
      case inferDiagPos ts d of
        Just p -> d { diagPos = Just p }
        Nothing -> d

inferDiagPos :: [Token] -> Diagnostic -> Maybe Position
inferDiagPos toks d =
  case diagCode d of
    "SEM_VAR_UNDECL" ->
      extractAfter "Undeclared variable: " (diagMessage d) >>= findIdentifierPos toks
    "SEM_CALL_UNKNOWN" ->
      extractAfter "Unknown function: " (diagMessage d) >>= findIdentifierPos toks
    "SEM_STRUCT_UNKNOWN" ->
      extractAfter "Unknown struct: " (diagMessage d) >>= findIdentifierPos toks
    "SEM_FIELD_UNKNOWN" ->
      extractFieldName (diagMessage d) >>= findFieldPos toks
    _ -> Nothing

attachRuntimePosition :: [Token] -> String -> String
attachRuntimePosition toks msg
  | "line " `isInfixOf` msg = msg
  | otherwise =
      case inferRuntimePos toks msg of
        Just (Position l c) -> msg ++ " [line " ++ show l ++ ", col " ++ show c ++ "]"
        Nothing -> msg

inferRuntimePos :: [Token] -> String -> Maybe Position
inferRuntimePos toks msg
  | "Undeclared variable: " `isInfixOf` msg =
      extractAfter "Undeclared variable: " msg >>= findIdentifierPos toks
  | "Unknown function: " `isInfixOf` msg =
      extractAfter "Unknown function: " msg >>= findIdentifierPos toks
  | "Unknown struct: " `isInfixOf` msg =
      extractAfter "Unknown struct: " msg >>= findIdentifierPos toks
  | "Array index out of bounds" `isInfixOf` msg =
      firstBracketPos toks
  | otherwise = Nothing

extractAfter :: String -> String -> Maybe String
extractAfter prefix s = do
  rest <- stripPrefix prefix s
  pure (takeWhile (\ch -> ch /= ' ' && ch /= '\n' && ch /= '\r' && ch /= ']') rest)

extractFieldName :: String -> Maybe String
extractFieldName s = do
  rest <- stripPrefix "Unknown field " s
  pure (takeWhile (\ch -> ch /= ' ') rest)

findIdentifierPos :: [Token] -> String -> Maybe Position
findIdentifierPos toks name = go toks
  where
    go [] = Nothing
    go (Identifier p n : xs)
      | n == name = Just p
      | otherwise = go xs
    go (_:xs) = go xs

findFieldPos :: [Token] -> String -> Maybe Position
findFieldPos toks field = go toks
  where
    go [] = Nothing
    go (Dot _ : Identifier p n : xs)
      | n == field = Just p
      | otherwise = go (Identifier p n : xs)
    go (_:xs) = go xs

firstBracketPos :: [Token] -> Maybe Position
firstBracketPos = go
  where
    go [] = Nothing
    go (LBracket p : _) = Just p
    go (_:xs) = go xs
