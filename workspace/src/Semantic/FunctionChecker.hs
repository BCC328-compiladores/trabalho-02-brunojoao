module Semantic.FunctionChecker
  ( checkFunctions
  ) where

import AST.AST (Program)
import Errors.Diagnostic (Diagnostic)
import Semantic.Analyzer (analyzeProgram)

-- | Faixada de checagem de funcoes.
-- O objetivo e manter fronteira de modulo para evoluir para passes dedicados
-- sem quebrar a API externa.
checkFunctions :: Program -> Either [Diagnostic] ()
checkFunctions p = do
  _ <- analyzeProgram p
  pure ()