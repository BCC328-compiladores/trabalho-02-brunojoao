module Errors.Pretty
  ( renderDiagnostic
  , renderDiagnostics
  ) where

import Errors.Diagnostic
import Token.Token (Position(..))

renderDiagnostic :: Diagnostic -> String
renderDiagnostic d =
  header ++ loc ++ "\n" ++ diagMessage d ++ hintPart
  where
    header = "[" ++ show (diagSeverity d) ++ "][" ++ diagCode d ++ "] "
    loc = maybe "" fmtPos (diagPos d)
    hintPart = maybe "" (\h -> "\nHint: " ++ h) (diagHint d)

fmtPos :: Position -> String
fmtPos (Position l c) = "line " ++ show l ++ ", col " ++ show c

renderDiagnostics :: [Diagnostic] -> String
renderDiagnostics = unlines . map renderDiagnostic