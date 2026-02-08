module TypeChecker.Infer
  ( inferVarDeclType
  ) where

import AST.AST (VarDecl(..))
import TypeChecker.Types (SType)

inferVarDeclType :: VarDecl -> Maybe SType -> Either String SType
inferVarDeclType v inferred =
  case inferred of
    Just t -> Right t
    Nothing -> Left ("Cannot infer type for variable: " ++ varName v)