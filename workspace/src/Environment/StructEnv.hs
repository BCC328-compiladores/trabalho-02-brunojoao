module Environment.StructEnv
  ( StructInfo(..)
  , StructEnv
  , empty
  , insertStruct
  , lookupStruct
  , hasStruct
  ) where

import AST.AST (StructField)
import qualified Data.Map.Strict as M

data StructInfo = StructInfo
  { siName   :: String
  , siFields :: [StructField]
  }
  deriving (Eq, Show)

newtype StructEnv = StructEnv (M.Map String StructInfo)
  deriving (Eq, Show)

empty :: StructEnv
empty = StructEnv M.empty

insertStruct :: StructInfo -> StructEnv -> Either String StructEnv
insertStruct si (StructEnv m)
  | M.member (siName si) m = Left ("Struct already declared: " ++ siName si)
  | otherwise = Right (StructEnv (M.insert (siName si) si m))

lookupStruct :: String -> StructEnv -> Maybe StructInfo
lookupStruct name (StructEnv m) = M.lookup name m

hasStruct :: String -> StructEnv -> Bool
hasStruct name env = maybe False (const True) (lookupStruct name env)