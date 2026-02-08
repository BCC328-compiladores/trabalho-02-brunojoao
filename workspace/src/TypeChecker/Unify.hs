module TypeChecker.Unify
  ( apply
  , applyMany
  , compose
  , unify
  ) where

import TypeChecker.Types
import qualified Data.Map.Strict as M

apply :: Subst -> SType -> SType
apply s t =
  case t of
    STArray a n -> STArray (apply s a) n
    STFun a b -> STFun (apply s a) (apply s b)
    STVar v ->
      case M.lookup v s of
        Just t' -> apply s t'
        Nothing -> STVar v
    _ -> t

applyMany :: Subst -> [SType] -> [SType]
applyMany s = map (apply s)

compose :: Subst -> Subst -> Subst
compose s1 s2 = M.map (apply s1) s2 `M.union` s1

occurs :: String -> SType -> Bool
occurs v t =
  case t of
    STVar x -> v == x
    STArray a _ -> occurs v a
    STFun a b -> occurs v a || occurs v b
    _ -> False

bind :: String -> SType -> Either String Subst
bind v t
  | t == STVar v = Right M.empty
  | occurs v t = Left ("Infinite type for " ++ v)
  | otherwise = Right (M.singleton v t)

unify :: SType -> SType -> Either String Subst
unify t1 t2 =
  case (t1, t2) of
    (STInt, STInt) -> Right M.empty
    (STFloat, STFloat) -> Right M.empty
    (STBool, STBool) -> Right M.empty
    (STString, STString) -> Right M.empty
    (STVoid, STVoid) -> Right M.empty
    (STStruct a, STStruct b)
      | a == b -> Right M.empty
      | otherwise -> Left ("Struct mismatch: " ++ a ++ " vs " ++ b)
    (STArray a n1, STArray b n2)
      | sizeOk n1 n2 -> unify a b
      | otherwise -> Left "Array size mismatch"
    (STFun a1 b1, STFun a2 b2) -> do
      s1 <- unify a1 a2
      s2 <- unify (apply s1 b1) (apply s1 b2)
      Right (compose s2 s1)
    (STVar v, t) -> bind v t
    (t, STVar v) -> bind v t
    _ -> Left ("Cannot unify " ++ show t1 ++ " with " ++ show t2)
  where
    sizeOk Nothing _ = True
    sizeOk _ Nothing = True
    sizeOk (Just a) (Just b) = a == b