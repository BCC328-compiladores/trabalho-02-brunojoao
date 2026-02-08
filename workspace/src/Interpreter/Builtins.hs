module Interpreter.Builtins
  ( isBuiltin
  ) where

isBuiltin :: String -> Bool
isBuiltin "print" = True
isBuiltin _ = False
