module Environment.TypeEnv
  ( TypeEnv
  , empty
  , enterScope
  , exitScope
  , declareType
  , lookupType
  ) where

import qualified Environment.SymbolTable as ST
import TypeChecker.Types (SType)

type TypeEnv = ST.SymbolTable SType

empty :: TypeEnv
empty = ST.empty

enterScope :: TypeEnv -> TypeEnv
enterScope = ST.enterScope

exitScope :: TypeEnv -> TypeEnv
exitScope = ST.exitScope

declareType :: String -> SType -> TypeEnv -> Either String TypeEnv
declareType = ST.declare

lookupType :: String -> TypeEnv -> Maybe SType
lookupType = ST.lookupSymbol