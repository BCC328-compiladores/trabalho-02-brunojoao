module Semantic.Analyzer
  ( CheckedProgram(..)
  , analyzeProgram
  ) where

import AST.AST
import Environment.FuncEnv (FuncEnv, FuncSig(..), insertFunc, lookupFunc)
import qualified Environment.FuncEnv as FE
import Environment.StructEnv (StructEnv, StructInfo(..), hasStruct, insertStruct, lookupStruct)
import qualified Environment.StructEnv as SE
import qualified Environment.TypeEnv as TE
import Errors.Diagnostic
import TypeChecker.Types
import TypeChecker.Unify
import qualified Data.List as L
import Data.Maybe (listToMaybe)
import qualified Data.Map.Strict as M

data CheckedProgram = CheckedProgram
  { cpProgram :: Program
  , cpStructs :: StructEnv
  , cpFuncs   :: FuncEnv
  }
  deriving (Eq, Show)

analyzeProgram :: Program -> Either [Diagnostic] CheckedProgram
analyzeProgram p@(Program decls) = do
  structs <- collectStructs decls
  funcs <- collectFunctions structs decls
  gEnv <- checkGlobals structs funcs decls
  _ <- collectAll (map (checkFunction structs funcs gEnv) (funcDecls decls))
  pure (CheckedProgram p structs funcs)

funcDecls :: [Decl] -> [FuncDecl]
funcDecls = foldr go []
  where
    go (DeclFunc f) xs = f : xs
    go _ xs = xs

varDecls :: [Decl] -> [VarDecl]
varDecls = foldr go []
  where
    go (DeclVar v) xs = v : xs
    go _ xs = xs

collectAll :: [Either Diagnostic a] -> Either [Diagnostic] [a]
collectAll xs =
  case partitionE xs of
    ([], ok) -> Right ok
    (errs, _) -> Left errs

partitionE :: [Either e a] -> ([e], [a])
partitionE = foldr go ([], [])
  where
    go (Left e) (es, as) = (e:es, as)
    go (Right a) (es, as) = (es, a:as)

diag :: String -> String -> Diagnostic
diag c msg = mkError c msg

collectStructs :: [Decl] -> Either [Diagnostic] StructEnv
collectStructs decls = foldl step (Right SE.empty) structs
  where
    structs = [s | DeclStruct s <- decls]
    step acc s = do
      env <- acc
      case insertStruct (StructInfo (structName s) (structFields s)) env of
        Left e -> Left [diag "SEM_STRUCT_DUP" e]
        Right env' -> Right env'

collectFunctions :: StructEnv -> [Decl] -> Either [Diagnostic] FuncEnv
collectFunctions senv decls = foldl step (Right FE.empty) funcs
  where
    funcs = [f | DeclFunc f <- decls]
    step acc f = do
      env <- acc
      sig <- makeSig senv f
      case insertFunc sig env of
        Left e -> Left [diag "SEM_FUNC_DUP" e]
        Right env' -> Right env'

makeSig :: StructEnv -> FuncDecl -> Either [Diagnostic] FuncSig
makeSig senv f = do
  ptys <- sequence (zipWith toParamTy [0 :: Int ..] (funcParams f))
  rty <- case funcRet f of
    Just t -> toSType senv t
    Nothing -> Right (STVar ("ret_" ++ funcName f))
  let tvars = L.nub (funcTParams f ++ collectTypeVars (rty : ptys))
  Right
    FuncSig
      { fsName = funcName f
      , fsTypeVars = tvars
      , fsParams = ptys
      , fsRet = rty
      , fsDecl = f
      }
  where
    toParamTy i (Param pname mty) =
      case mty of
        Just t -> toSType senv t
        Nothing -> Right (STVar ("p_" ++ funcName f ++ "_" ++ pname ++ "_" ++ show i))

collectTypeVars :: [SType] -> [String]
collectTypeVars = concatMap go
  where
    go (STVar v) = [v]
    go (STArray t _) = go t
    go (STFun a b) = go a ++ go b
    go _ = []

toSType :: StructEnv -> Type -> Either [Diagnostic] SType
toSType senv t =
  case t of
    TInt -> Right STInt
    TFloat -> Right STFloat
    TBool -> Right STBool
    TString -> Right STString
    TVoid -> Right STVoid
    TArray a n -> STArray <$> toSType senv a <*> pure n
    TNamed n
      | hasStruct n senv -> Right (STStruct n)
      | otherwise -> Right (STVar n)
    TFun a b ->
      STFun <$> toSType senv a <*> toSType senv b

checkGlobals :: StructEnv -> FuncEnv -> [Decl] -> Either [Diagnostic] TE.TypeEnv
checkGlobals senv fenv decls = foldl step (Right TE.empty) (varDecls decls)
  where
    step acc vd = do
      env <- acc
      ty <- checkVarDeclType senv fenv env vd
      case TE.declareType (varName vd) ty env of
        Left e -> Left [diag "SEM_GLOBAL_DUP" e]
        Right env' -> Right env'

checkFunction :: StructEnv -> FuncEnv -> TE.TypeEnv -> FuncDecl -> Either Diagnostic ()
checkFunction senv fenv gEnv f = do
  env0 <- pure (TE.enterScope gEnv)
  sig <- maybe (Left (diag "SEM_FUNC_MISSING" ("Missing function in env: " ++ funcName f))) Right (lookupFunc (funcName f) fenv)
  env1 <- addParams env0 (zip (funcParams f) (fsParams sig))
  (_, rets) <- checkBlock senv fenv (fsRet sig) env1 (funcBody f)
  case funcRet f of
    Just _ -> do
      _ <- mapM_ (ensureRet (fsRet sig)) rets
      if fsRet sig /= STVoid && null rets
        then Left (diag "SEM_RETURN_REQUIRED" ("Function requires return: " ++ funcName f))
        else Right ()
    Nothing ->
      checkInferredReturns rets

ensureRet :: SType -> SType -> Either Diagnostic ()
ensureRet expected actual =
  case unify expected actual of
    Left e -> Left (diag "SEM_RETURN_TYPE" e)
    Right _ -> Right ()

checkInferredReturns :: [SType] -> Either Diagnostic ()
checkInferredReturns [] = Right ()
checkInferredReturns (x:xs) = mapM_ (sameAsFirst x) xs
  where
    sameAsFirst first t =
      case unify first t of
        Left _ -> Left (diag "SEM_RETURN_INFER" "Inferred return types are inconsistent in function body")
        Right _ -> Right ()

addParams :: TE.TypeEnv -> [(Param, SType)] -> Either Diagnostic TE.TypeEnv
addParams env [] = Right env
addParams env ((Param pname _, pty):rest) =
  case TE.declareType pname pty env of
    Left e -> Left (diag "SEM_PARAM_DUP" e)
    Right env' -> addParams env' rest

checkBlock :: StructEnv -> FuncEnv -> SType -> TE.TypeEnv -> Block -> Either Diagnostic (TE.TypeEnv, [SType])
checkBlock senv fenv retTy env stmts = go env [] stmts
  where
    go e rs [] = Right (e, reverse rs)
    go e rs (x:xs) = do
      (e', mret) <- checkStmt senv fenv retTy e x
      let rs' = maybe rs (:rs) mret
      go e' rs' xs

checkStmt :: StructEnv -> FuncEnv -> SType -> TE.TypeEnv -> Stmt -> Either Diagnostic (TE.TypeEnv, Maybe SType)
checkStmt senv fenv retTy env stmt =
  case stmt of
    StmtVar vd -> do
      ty <- case checkVarDeclType senv fenv env vd of
        Left ds -> Left (head ds)
        Right t -> Right t
      env' <- case TE.declareType (varName vd) ty env of
        Left e -> Left (diag "SEM_VAR_DUP" e)
        Right x -> Right x
      Right (env', Nothing)
    StmtAssign lv ex -> do
      lty <- inferLValue senv fenv env lv
      ety <- inferExpr senv fenv env ex
      matchOrFail "SEM_ASSIGN_TYPE" lty ety
      Right (env, Nothing)
    StmtIf c t me -> do
      cty <- inferExpr senv fenv env c
      matchOrFail "SEM_IF_COND" STBool cty
      (_, rt) <- checkBlock senv fenv retTy (TE.enterScope env) t
      re <- case me of
        Nothing -> Right []
        Just b -> snd <$> checkBlock senv fenv retTy (TE.enterScope env) b
      Right (env, listToMaybe (rt ++ re))
    StmtWhile c b -> do
      cty <- inferExpr senv fenv env c
      matchOrFail "SEM_WHILE_COND" STBool cty
      _ <- checkBlock senv fenv retTy (TE.enterScope env) b
      Right (env, Nothing)
    StmtFor mi c u b -> do
      let loopEnvBase = TE.enterScope env
      env1 <- case mi of
        Nothing -> Right loopEnvBase
        Just (AssignExpr lv ex) -> do
          ety <- inferExpr senv fenv loopEnvBase ex
          case lv of
            LVar n ->
              case TE.lookupType n loopEnvBase of
                Nothing ->
                  case TE.declareType n ety loopEnvBase of
                    Left e -> Left (diag "SEM_FOR_INIT" e)
                    Right env' -> Right env'
                Just lty -> do
                  matchOrFail "SEM_FOR_INIT" lty ety
                  Right loopEnvBase
            _ -> do
              lty <- inferLValue senv fenv loopEnvBase lv
              matchOrFail "SEM_FOR_INIT" lty ety
              Right loopEnvBase
      cty <- inferExpr senv fenv env1 c
      matchOrFail "SEM_FOR_COND" STBool cty
      case u of
        EPostInc lv -> do
          uty <- inferLValue senv fenv env1 lv
          if isNumeric uty
            then pure ()
            else Left (diag "SEM_FOR_UPDATE" "for update using i++ requires numeric lvalue")
        EVar lv -> do
          uty <- inferLValue senv fenv env1 lv
          if isNumeric uty
            then pure ()
            else Left (diag "SEM_FOR_UPDATE" "for update using i++ requires numeric lvalue")
        _ -> do
          _ <- inferExpr senv fenv env1 u
          pure ()
      _ <- checkBlock senv fenv retTy (TE.enterScope env1) b
      Right (env, Nothing)
    StmtReturn me -> do
      rty <- case me of
        Nothing -> Right STVoid
        Just e -> inferExpr senv fenv env e
      matchOrFail "SEM_RETURN_TYPE" retTy rty
      Right (env, Just rty)
    StmtExpr e -> do
      _ <- inferExpr senv fenv env e
      Right (env, Nothing)
    StmtInc lv -> do
      lty <- inferLValue senv fenv env lv
      if isNumeric lty
        then Right (env, Nothing)
        else Left (diag "SEM_INC_TYPE" "Increment requires int or float")

checkVarDeclType :: StructEnv -> FuncEnv -> TE.TypeEnv -> VarDecl -> Either [Diagnostic] SType
checkVarDeclType senv fenv env vd = do
  mdecl <- case varType vd of
    Nothing -> Right Nothing
    Just t -> Just <$> toSType senv t
  minit <- case varInit vd of
    Nothing -> Right Nothing
    Just e ->
      case inferExpr senv fenv env e of
        Left d -> Left [d]
        Right t -> Right (Just t)
  case (mdecl, minit) of
    (Nothing, Nothing) ->
      Left [diag "SEM_VAR_INFER" ("Cannot infer type for variable: " ++ varName vd)]
    (Nothing, Just ity) ->
      Right ity
    (Just dty, Nothing) ->
      Right dty
    (Just dty, Just ity) ->
      case unify dty ity of
        Left e -> Left [diag "SEM_VAR_TYPE" e]
        Right _ -> Right dty

inferExpr :: StructEnv -> FuncEnv -> TE.TypeEnv -> Expr -> Either Diagnostic SType
inferExpr senv fenv env ex =
  case ex of
    EOr a b -> boolBin "SEM_OR" a b
    EAnd a b -> boolBin "SEM_AND" a b
    ERel op a b -> do
      ta <- inferExpr senv fenv env a
      tb <- inferExpr senv fenv env b
      case op of
        AEq -> relEq ta tb
        ANe -> relEq ta tb
        _ ->
          if isNumeric ta && isNumeric tb
            then Right STBool
            else Left (diag "SEM_REL_NUM" "Relational operators require numeric operands")
    EAdd _ a b -> numBin "SEM_ADD" a b
    EMul _ a b -> numBin "SEM_MUL" a b
    EPostInc lv -> do
      t <- inferLValue senv fenv env lv
      if isNumeric t
        then Right t
        else Left (diag "SEM_INC_TYPE" "Increment requires int or float")
    ECall fname args -> inferCall senv fenv env fname args
    EVar lv -> inferLValue senv fenv env lv
    ELit lit -> Right (litTy lit)
    EArrayLit xs ->
      case xs of
        [] -> Right (STArray (STVar "a") (Just 0))
        (x:rest) -> do
          t0 <- inferExpr senv fenv env x
          ts <- mapM (inferExpr senv fenv env) rest
          _ <- mapM (matchOrFail "SEM_ARRAY_ELEM" t0) ts
          Right (STArray t0 (Just (length xs)))
    EStructInit sname args -> do
      sinfo <- maybe (Left (diag "SEM_STRUCT_UNKNOWN" ("Unknown struct: " ++ sname))) Right (lookupStruct sname senv)
      let fields = siFields sinfo
      if length fields /= length args
        then Left (diag "SEM_STRUCT_ARITY" ("Struct " ++ sname ++ " expects " ++ show (length fields) ++ " fields"))
        else pure ()
      let ftypes = [t | StructField _ t <- fields]
      stypes <- case mapM (toSType senv) ftypes of
        Left ds -> Left (head ds)
        Right ts -> Right ts
      atypes <- mapM (inferExpr senv fenv env) args
      _ <- sequence_ (zipWith (matchOrFail "SEM_STRUCT_FIELD") stypes atypes)
      Right (STStruct sname)
    ENew t sz -> do
      et <- case toSType senv t of
        Left ds -> Left (head ds)
        Right x -> Right x
      szt <- inferExpr senv fenv env sz
      matchOrFail "SEM_NEW_SIZE" STInt szt
      Right (STArray et Nothing)
    EParen e -> inferExpr senv fenv env e
    EArraySize e -> do
      t <- inferExpr senv fenv env e
      case t of
        STArray _ _ -> Right STInt
        _ -> Left (diag "SEM_SIZE_TARGET" ".size requires an array")
  where
    boolBin code a b = do
      ta <- inferExpr senv fenv env a
      tb <- inferExpr senv fenv env b
      matchOrFail code STBool ta
      matchOrFail code STBool tb
      Right STBool

    numBin code a b = do
      ta <- inferExpr senv fenv env a
      tb <- inferExpr senv fenv env b
      if isNumeric ta && isNumeric tb
        then Right (if ta == STFloat || tb == STFloat then STFloat else STInt)
        else Left (diag code "Numeric operation requires int/float operands")

    relEq ta tb =
      case unify ta tb of
        Left _ ->
          if isNumeric ta && isNumeric tb
            then Right STBool
            else Left (diag "SEM_REL_EQ" "Equality compares values of compatible types")
        Right _ -> Right STBool

inferCall :: StructEnv -> FuncEnv -> TE.TypeEnv -> String -> [Expr] -> Either Diagnostic SType
inferCall senv fenv env fname args
  | fname == "print" = do
      _ <- mapM (inferExpr senv fenv env) args
      Right STVoid
  | otherwise = do
      case lookupFunc fname fenv of
        Just sig -> do
          if length args /= length (fsParams sig)
            then Left (diag "SEM_CALL_ARITY" ("Function " ++ fname ++ " expects " ++ show (length (fsParams sig)) ++ " args"))
            else pure ()
          atys <- mapM (inferExpr senv fenv env) args
          sub <- unifyMany (fsParams sig) atys
          let rty = apply sub (fsRet sig)
          Right rty
        Nothing ->
          case TE.lookupType fname env of
            Nothing -> Left (diag "SEM_CALL_UNKNOWN" ("Unknown function: " ++ fname))
            Just fty -> do
              atys <- mapM (inferExpr senv fenv env) args
              applyFunArgs fty atys

applyFunArgs :: SType -> [SType] -> Either Diagnostic SType
applyFunArgs fty atys = go fty atys
  where
    go t [] = Right t
    go (STFun a b) (x:xs) = do
      s <- case unify a x of
        Left e -> Left (diag "SEM_CALL_ARG_TYPE" e)
        Right sub -> Right sub
      go (apply s b) xs
    go _ (_:_) = Left (diag "SEM_CALL_NOT_FUN" "Attempted to call a non-function value")

unifyMany :: [SType] -> [SType] -> Either Diagnostic Subst
unifyMany ps as = go M.empty ps as
  where
    go s [] [] = Right s
    go s (p:pt) (a:at) = do
      let p' = apply s p
      let a' = apply s a
      s1 <- case unify p' a' of
        Left e -> Left (diag "SEM_CALL_ARG_TYPE" e)
        Right u -> Right u
      go (compose s1 s) pt at
    go _ _ _ = Left (diag "SEM_INTERNAL" "arity mismatch in unifyMany")

inferLValue :: StructEnv -> FuncEnv -> TE.TypeEnv -> LValue -> Either Diagnostic SType
inferLValue senv fenv env lv =
  case lv of
    LVar n ->
      maybe (Left (diag "SEM_VAR_UNDECL" ("Undeclared variable: " ++ n))) Right (TE.lookupType n env)
    LField base fname -> do
      bty <- inferLValue senv fenv env base
      case bty of
        STArray _ _ | fname == "size" ->
          Right STInt
        STStruct sn -> do
          si <- maybe (Left (diag "SEM_STRUCT_UNKNOWN" ("Unknown struct: " ++ sn))) Right (lookupStruct sn senv)
          case [t | StructField fn t <- siFields si, fn == fname] of
            [] -> Left (diag "SEM_FIELD_UNKNOWN" ("Unknown field " ++ fname ++ " in " ++ sn))
            (t:_) -> case toSType senv t of
              Left ds -> Left (head ds)
              Right x -> Right x
        _ -> Left (diag "SEM_FIELD_TARGET" "Field access requires struct value")
    LIndex base idx -> do
      bty <- inferLValue senv fenv env base
      ity <- inferExpr senv fenv env idx
      matchOrFail "SEM_INDEX_TYPE" STInt ity
      case bty of
        STArray t _ -> Right t
        _ -> Left (diag "SEM_INDEX_TARGET" "Indexing requires array value")

matchOrFail :: String -> SType -> SType -> Either Diagnostic ()
matchOrFail code expected actual =
  case unify expected actual of
    Left _ ->
      Left (diag code ("Expected " ++ prettySType expected ++ ", found " ++ prettySType actual))
    Right _ -> Right ()

litTy :: Literal -> SType
litTy lit =
  case lit of
    LInt _ -> STInt
    LFloat _ -> STFloat
    LBool _ -> STBool
    LString _ -> STString