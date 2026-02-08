module Semantic.StructChecker
  ( checkStructs
  ) where

import AST.AST (Program)
import Errors.Diagnostic (Diagnostic)
import Semantic.Analyzer (analyzeProgram)

-- | Faixada de checagem de structs.
-- Hoje delega ao analisador central; foi mantido para deixar a arquitetura
-- pronta para separacao de passes no Stage 3.
checkStructs :: Program -> Either [Diagnostic] ()
checkStructs p = do
  _ <- analyzeProgram p
  pure ()