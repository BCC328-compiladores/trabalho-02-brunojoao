module Interpreter.Value
  ( Value(..)
  , showValue
  ) where

import qualified Data.Map.Strict as M

data Value
  = VInt Int
  | VFloat Double
  | VBool Bool
  | VString String
  | VArray [Value]
  | VStruct String (M.Map String Value)
  | VVoid
  deriving (Eq, Show)

showValue :: Value -> String
showValue v =
  case v of
    VInt i -> show i
    VFloat f -> show f
    VBool b -> show b
    VString s -> s
    VArray xs -> "[" ++ unwords (map showValue xs) ++ "]"
    VStruct n _ -> "<struct " ++ n ++ ">"
    VVoid -> "void"