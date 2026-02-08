module Environment.FuncEnv
  ( FuncSig(..)
  , FuncEnv
  , empty
  , insertFunc
  , lookupFunc
  ) where

import AST.AST (FuncDecl)
import TypeChecker.Types (SType)
import qualified Data.Map.Strict as M

data FuncSig = FuncSig
  { fsName     :: String
  , fsTypeVars :: [String]
  , fsParams   :: [SType]
  , fsRet      :: SType
  , fsDecl     :: FuncDecl
  }
  deriving (Eq, Show)

newtype FuncEnv = FuncEnv (M.Map String FuncSig)
  deriving (Eq, Show)

empty :: FuncEnv
empty = FuncEnv M.empty

insertFunc :: FuncSig -> FuncEnv -> Either String FuncEnv
insertFunc sig (FuncEnv m)
  | M.member (fsName sig) m = Left ("Function already declared: " ++ fsName sig)
  | otherwise = Right (FuncEnv (M.insert (fsName sig) sig m))

lookupFunc :: String -> FuncEnv -> Maybe FuncSig
lookupFunc name (FuncEnv m) = M.lookup name m