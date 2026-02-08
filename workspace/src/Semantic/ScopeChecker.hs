module Semantic.ScopeChecker
  ( checkScopes
  ) where

import AST.AST (Program)
import Errors.Diagnostic (Diagnostic)
import Semantic.Analyzer (analyzeProgram)

-- | Faixada de checagem de escopo.
-- Mantemos este modulo separado para preservar a arquitetura por passes,
-- mesmo com a implementacao atual centralizada em 'analyzeProgram'.
checkScopes :: Program -> Either [Diagnostic] ()
checkScopes p = do
  _ <- analyzeProgram p
  pure ()