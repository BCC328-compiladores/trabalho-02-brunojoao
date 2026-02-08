module Environment.SymbolTable
  ( SymbolTable
  , empty
  , enterScope
  , exitScope
  , declare
  , assign
  , lookupSymbol
  ) where

import qualified Data.Map.Strict as M

newtype SymbolTable a = SymbolTable [M.Map String a]
  deriving (Eq, Show)

empty :: SymbolTable a
empty = SymbolTable [M.empty]

enterScope :: SymbolTable a -> SymbolTable a
enterScope (SymbolTable scopes) = SymbolTable (M.empty : scopes)

exitScope :: SymbolTable a -> SymbolTable a
exitScope (SymbolTable (_:rest@(_:_))) = SymbolTable rest
exitScope st = st

declare :: String -> a -> SymbolTable a -> Either String (SymbolTable a)
declare name val (SymbolTable (scope:rest))
  | M.member name scope = Left ("Symbol already declared in scope: " ++ name)
  | otherwise = Right (SymbolTable (M.insert name val scope : rest))
declare _ _ (SymbolTable []) = Left "Invalid symbol table state"

assign :: String -> a -> SymbolTable a -> Either String (SymbolTable a)
assign name val (SymbolTable scopes) =
  case go scopes of
    Nothing -> Left ("Undeclared symbol: " ++ name)
    Just s  -> Right (SymbolTable s)
  where
    go [] = Nothing
    go (x:xs)
      | M.member name x = Just (M.insert name val x : xs)
      | otherwise = do
          rest <- go xs
          pure (x : rest)

lookupSymbol :: String -> SymbolTable a -> Maybe a
lookupSymbol name (SymbolTable scopes) = go scopes
  where
    go [] = Nothing
    go (x:xs) =
      case M.lookup name x of
        Just v  -> Just v
        Nothing -> go xs