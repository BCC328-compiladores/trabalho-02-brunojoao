module TypeChecker.Types
  ( SType(..)
  , Subst
  , isNumeric
  , prettySType
  ) where

import qualified Data.Map.Strict as M

data SType
  = STInt
  | STFloat
  | STBool
  | STString
  | STVoid
  | STArray SType (Maybe Int)
  | STFun SType SType
  | STStruct String
  | STVar String
  deriving (Eq, Ord, Show)

type Subst = M.Map String SType

isNumeric :: SType -> Bool
isNumeric STInt = True
isNumeric STFloat = True
isNumeric _ = False

prettySType :: SType -> String
prettySType STInt = "int"
prettySType STFloat = "float"
prettySType STBool = "bool"
prettySType STString = "string"
prettySType STVoid = "void"
prettySType (STArray t Nothing) = prettySType t ++ "[]"
prettySType (STArray t (Just n)) = prettySType t ++ "[" ++ show n ++ "]"
prettySType (STFun a b) = "(" ++ prettySType a ++ ") -> " ++ prettySType b
prettySType (STStruct n) = n
prettySType (STVar v) = v