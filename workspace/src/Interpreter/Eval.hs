module Interpreter.Eval
  ( evalProgram
  ) where

import AST.AST
import Interpreter.Builtins (isBuiltin)
import Interpreter.Value
import Semantic.Analyzer (CheckedProgram(..))
import qualified Data.Map.Strict as M
import Control.Monad (when)
import Control.Monad.State.Strict
import Control.Monad.Except

type Scope = M.Map String Value

data Runtime = Runtime
  { rtScopes  :: [Scope]
  , rtFuncs   :: M.Map String FuncDecl
  , rtStructs :: M.Map String StructDecl
  , rtOutput  :: [String]
  }

type Eval a = ExceptT String (State Runtime) a

evalProgram :: CheckedProgram -> Either String [String]
evalProgram checked =
  case runState (runExceptT run) initial of
    (Left e, _) -> Left e
    (Right (), finalRt) -> Right (reverse (rtOutput finalRt))
  where
    prog = cpProgram checked
    initial = Runtime [M.empty] M.empty M.empty []
    run = do
      loadProgram prog
      _ <- callFunction "main" []
      pure ()

loadProgram :: Program -> Eval ()
loadProgram (Program decls) = do
  mapM_ loadStruct [s | DeclStruct s <- decls]
  mapM_ loadFunc [f | DeclFunc f <- decls]
  mapM_ loadGlobal [v | DeclVar v <- decls]

loadStruct :: StructDecl -> Eval ()
loadStruct s = modify (\rt -> rt { rtStructs = M.insert (structName s) s (rtStructs rt) })

loadFunc :: FuncDecl -> Eval ()
loadFunc f = modify (\rt -> rt { rtFuncs = M.insert (funcName f) f (rtFuncs rt) })

loadGlobal :: VarDecl -> Eval ()
loadGlobal vd = do
  v <- case varInit vd of
    Nothing -> pure VVoid
    Just e -> evalExpr e
  declareVar (varName vd) v

evalBlock :: Block -> Eval (Maybe Value)
evalBlock [] = pure Nothing
evalBlock (s:ss) = do
  r <- evalStmt s
  case r of
    Just v -> pure (Just v)
    Nothing -> evalBlock ss

evalStmt :: Stmt -> Eval (Maybe Value)
evalStmt stmt =
  case stmt of
    StmtVar vd -> do
      v <- case varInit vd of
        Nothing -> pure VVoid
        Just e -> evalExpr e
      declareVar (varName vd) v
      pure Nothing
    StmtAssign lv e -> do
      v <- evalExpr e
      setLValue lv v
      pure Nothing
    StmtIf c t me -> do
      cv <- evalExpr c >>= expectBool
      if cv
        then withScope (evalBlock t)
        else case me of
               Nothing -> pure Nothing
               Just b -> withScope (evalBlock b)
    StmtWhile c b -> loop
      where
        loop = do
          cv <- evalExpr c >>= expectBool
          if not cv
            then pure Nothing
            else do
              r <- withScope (evalBlock b)
              case r of
                Just v -> pure (Just v)
                Nothing -> loop
    StmtFor mi c u b -> do
      withScope $ do
        case mi of
          Nothing -> pure ()
          Just (AssignExpr lv e) -> do
            v <- evalExpr e
            assignOrDeclare lv v
        loop
      where
        loop = do
          cv <- evalExpr c >>= expectBool
          if not cv
            then pure Nothing
            else do
              r <- withScope (evalBlock b)
              case r of
                Just v -> pure (Just v)
                Nothing -> do
                  runUpdate u
                  loop
    StmtReturn me -> do
      v <- maybe (pure VVoid) evalExpr me
      pure (Just v)
    StmtExpr e -> do
      _ <- evalExpr e
      pure Nothing
    StmtInc lv -> do
      curr <- getLValue lv
      next <- incValue curr
      setLValue lv next
      pure Nothing

assignOrDeclare :: LValue -> Value -> Eval ()
assignOrDeclare lv v =
  case lv of
    LVar n -> do
      found <- lookupMaybe n
      case found of
        Just _ -> setVar n v
        Nothing -> declareVar n v
    _ -> setLValue lv v

runUpdate :: Expr -> Eval ()
runUpdate e =
  case e of
    EPostInc lv -> do
      curr <- getLValue lv
      next <- incValue curr
      setLValue lv next
    -- Compatibilidade com AST antigo.
    EVar lv -> do
      curr <- getLValue lv
      next <- incValue curr
      setLValue lv next
    _ -> do
      _ <- evalExpr e
      pure ()

evalExpr :: Expr -> Eval Value
evalExpr ex =
  case ex of
    EOr a b -> do
      av <- evalExpr a >>= expectBool
      if av then pure (VBool True) else VBool <$> (evalExpr b >>= expectBool)
    EAnd a b -> do
      av <- evalExpr a >>= expectBool
      if not av then pure (VBool False) else VBool <$> (evalExpr b >>= expectBool)
    ERel op a b -> do
      va <- evalExpr a
      vb <- evalExpr b
      VBool <$> evalRel op va vb
    EAdd op a b -> do
      va <- evalExpr a
      vb <- evalExpr b
      evalAdd op va vb
    EMul op a b -> do
      va <- evalExpr a
      vb <- evalExpr b
      evalMul op va vb
    EPostInc lv -> do
      curr <- getLValue lv
      next <- incValue curr
      setLValue lv next
      pure next
    ECall fname args
      | isBuiltin fname -> callBuiltin fname args
      | otherwise -> do
          avs <- mapM evalExpr args
          callFunction fname avs
    EVar lv -> getLValue lv
    ELit lit -> pure (litToValue lit)
    EArrayLit xs -> VArray <$> mapM evalExpr xs
    EStructInit sname args -> do
      sd <- getStruct sname
      vals <- mapM evalExpr args
      let fields = structFields sd
      if length fields /= length vals
        then throwError ("Struct initializer arity mismatch for " ++ sname)
        else do
          let pairs = zipWith (\(StructField n _) v -> (n, v)) fields vals
          pure (VStruct sname (M.fromList pairs))
    ENew t e -> do
      n <- evalExpr e >>= expectInt
      if n < 0 then throwError "Negative array size" else do
        let val = defaultFor t
        pure (VArray (replicate n val))
    EParen e -> evalExpr e
    EArraySize e -> do
      v <- evalExpr e
      case v of
        VArray xs -> pure (VInt (length xs))
        _ -> throwError ".size requires array"

callBuiltin :: String -> [Expr] -> Eval Value
callBuiltin "print" args = do
  vals <- mapM evalExpr args
  let out = unwords (map showValue vals)
  modify (\rt -> rt { rtOutput = out : rtOutput rt })
  pure VVoid
callBuiltin n _ = throwError ("Unknown builtin: " ++ n)

callFunction :: String -> [Value] -> Eval Value
callFunction name args = do
  fn <- getFunc name
  let params = funcParams fn
  when (length params /= length args) $
    throwError ("Function arity mismatch: " ++ name)
  withScope $ do
    mapM_ bind (zip params args)
    r <- evalBlock (funcBody fn)
    pure (maybe VVoid id r)
  where
    bind (Param pname _, v) = declareVar pname v

defaultFor :: Type -> Value
defaultFor t =
  case t of
    TInt -> VInt 0
    TFloat -> VFloat 0
    TBool -> VBool False
    TString -> VString ""
    TVoid -> VVoid
    TArray _ _ -> VArray []
    TNamed n -> VStruct n M.empty
    TFun _ _ -> VVoid

litToValue :: Literal -> Value
litToValue lit =
  case lit of
    LInt i -> VInt i
    LFloat f -> VFloat f
    LBool b -> VBool b
    LString s -> VString s

evalRel :: RelOp -> Value -> Value -> Eval Bool
evalRel op a b =
  case (a, b) of
    (VInt x, VInt y) -> pure (cmp (fromIntegral x :: Double) (fromIntegral y))
    (VFloat x, VFloat y) -> pure (cmp x y)
    (VInt x, VFloat y) -> pure (cmp (fromIntegral x) y)
    (VFloat x, VInt y) -> pure (cmp x (fromIntegral y))
    (VBool x, VBool y) ->
      case op of
        AEq -> pure (x == y)
        ANe -> pure (x /= y)
        _ -> throwError "Invalid boolean relational operator"
    (VString x, VString y) ->
      case op of
        AEq -> pure (x == y)
        ANe -> pure (x /= y)
        _ -> throwError "Invalid string relational operator"
    _ -> throwError "Invalid operands for relational expression"
  where
    cmp x y =
      case op of
        AEq -> x == y
        ANe -> x /= y
        ALt -> x < y
        AGt -> x > y
        ALe -> x <= y
        AGe -> x >= y

evalAdd :: AddOp -> Value -> Value -> Eval Value
evalAdd op a b =
  case (a, b) of
    (VInt x, VInt y) ->
      pure (VInt (if op == APlus then x + y else x - y))
    (VFloat x, VFloat y) ->
      pure (VFloat (if op == APlus then x + y else x - y))
    (VInt x, VFloat y) ->
      pure (VFloat (if op == APlus then fromIntegral x + y else fromIntegral x - y))
    (VFloat x, VInt y) ->
      pure (VFloat (if op == APlus then x + fromIntegral y else x - fromIntegral y))
    (VString x, VString y) | op == APlus ->
      pure (VString (x ++ y))
    _ -> throwError "Invalid operands for add/sub expression"

evalMul :: MulOp -> Value -> Value -> Eval Value
evalMul op a b =
  case (a, b) of
    (VInt x, VInt y) ->
      pure (VInt (if op == AMul then x * y else x `div` y))
    (VFloat x, VFloat y) ->
      pure (VFloat (if op == AMul then x * y else x / y))
    (VInt x, VFloat y) ->
      pure (VFloat (if op == AMul then fromIntegral x * y else fromIntegral x / y))
    (VFloat x, VInt y) ->
      pure (VFloat (if op == AMul then x * fromIntegral y else x / fromIntegral y))
    _ -> throwError "Invalid operands for mul/div expression"

expectBool :: Value -> Eval Bool
expectBool (VBool b) = pure b
expectBool _ = throwError "Expected bool value"

expectInt :: Value -> Eval Int
expectInt (VInt i) = pure i
expectInt _ = throwError "Expected int value"

incValue :: Value -> Eval Value
incValue (VInt i) = pure (VInt (i + 1))
incValue (VFloat f) = pure (VFloat (f + 1))
incValue _ = throwError "Increment requires int or float"

withScope :: Eval a -> Eval a
withScope action = do
  modify (\rt -> rt { rtScopes = M.empty : rtScopes rt })
  r <- action
  modify (\rt -> rt { rtScopes = drop 1 (rtScopes rt) })
  pure r

declareVar :: String -> Value -> Eval ()
declareVar name v = do
  rt <- get
  case rtScopes rt of
    [] -> throwError "Invalid runtime scope stack"
    (s:ss)
      | M.member name s -> throwError ("Variable already declared in scope: " ++ name)
      | otherwise -> put rt { rtScopes = M.insert name v s : ss }

lookupVar :: String -> Eval Value
lookupVar name = do
  scopes <- gets rtScopes
  case findIn scopes of
    Nothing -> throwError ("Undeclared variable: " ++ name)
    Just v -> pure v
  where
    findIn [] = Nothing
    findIn (s:ss) =
      case M.lookup name s of
        Just v -> Just v
        Nothing -> findIn ss

lookupMaybe :: String -> Eval (Maybe Value)
lookupMaybe name = do
  scopes <- gets rtScopes
  pure (findIn scopes)
  where
    findIn [] = Nothing
    findIn (s:ss) =
      case M.lookup name s of
        Just v -> Just v
        Nothing -> findIn ss

setVar :: String -> Value -> Eval ()
setVar name val = do
  rt <- get
  scopes' <- go (rtScopes rt)
  put rt { rtScopes = scopes' }
  where
    go :: [Scope] -> Eval [Scope]
    go [] = throwError ("Undeclared variable: " ++ name)
    go (s:ss)
      | M.member name s = pure (M.insert name val s : ss)
      | otherwise = do
          rest <- go ss
          pure (s : rest)

data Access
  = AField String
  | AIndex Expr

data ResolvedAccess
  = RField String
  | RIndex Int

decompose :: LValue -> (String, [Access])
decompose lv =
  case lv of
    LVar n -> (n, [])
    LField b f ->
      let (root, acc) = decompose b
      in (root, acc ++ [AField f])
    LIndex b e ->
      let (root, acc) = decompose b
      in (root, acc ++ [AIndex e])

resolveAccesses :: [Access] -> Eval [ResolvedAccess]
resolveAccesses = mapM go
  where
    go (AField f) = pure (RField f)
    go (AIndex e) = RIndex <$> (evalExpr e >>= expectInt)

getLValue :: LValue -> Eval Value
getLValue lv = do
  let (root, acc) = decompose lv
  base <- lookupVar root
  racc <- resolveAccesses acc
  getDeep base racc

setLValue :: LValue -> Value -> Eval ()
setLValue lv newVal = do
  let (root, acc) = decompose lv
  base <- lookupVar root
  racc <- resolveAccesses acc
  updated <- setDeep base racc newVal
  setVar root updated

getDeep :: Value -> [ResolvedAccess] -> Eval Value
getDeep v [] = pure v
getDeep v (a:as) =
  case (v, a) of
    (VArray xs, RField "size") ->
      getDeep (VInt (length xs)) as
    (VStruct _ fields, RField f) ->
      case M.lookup f fields of
        Nothing -> throwError ("Unknown struct field: " ++ f)
        Just x -> getDeep x as
    (VArray xs, RIndex i) ->
      if i < 0 || i >= length xs
        then throwError "Array index out of bounds"
        else getDeep (xs !! i) as
    _ -> throwError "Invalid lvalue access"

setDeep :: Value -> [ResolvedAccess] -> Value -> Eval Value
setDeep _ [] newVal = pure newVal
setDeep v (a:as) newVal =
  case (v, a) of
    (VArray _, RField "size") ->
      throwError "Array field size is read-only"
    (VStruct n fields, RField f) ->
      case M.lookup f fields of
        Nothing -> throwError ("Unknown struct field: " ++ f)
        Just cur -> do
          upd <- setDeep cur as newVal
          pure (VStruct n (M.insert f upd fields))
    (VArray xs, RIndex i) ->
      if i < 0 || i >= length xs
        then throwError "Array index out of bounds"
        else do
          upd <- setDeep (xs !! i) as newVal
          pure (VArray (replaceAt i upd xs))
    _ -> throwError "Invalid lvalue assignment"

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i val xs =
  let (l, r) = splitAt i xs
  in case r of
       [] -> xs
       (_:rs) -> l ++ (val : rs)

getFunc :: String -> Eval FuncDecl
getFunc name = do
  fm <- gets rtFuncs
  case M.lookup name fm of
    Just f -> pure f
    Nothing -> throwError ("Unknown function: " ++ name)

getStruct :: String -> Eval StructDecl
getStruct name = do
  sm <- gets rtStructs
  case M.lookup name sm of
    Just s -> pure s
    Nothing -> throwError ("Unknown struct: " ++ name)