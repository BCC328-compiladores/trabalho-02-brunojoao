module CodePretty (prettyProgram) where

import AST
import Prettyprinter hiding (semi, comma)

kw :: String -> Doc ann
kw = pretty

semi :: Doc ann
semi = pretty ";"

comma :: Doc ann
comma = pretty ","

bracesBlock :: [Doc ann] -> Doc ann
bracesBlock ds = braces (line <> indent 2 (vsep ds) <> line)

prettyProgram :: Program -> Doc ann
prettyProgram (Program decls) = vsep (map prettyDecl decls)

prettyDecl :: Decl -> Doc ann
prettyDecl (DeclFunc f)    = prettyFunc f
prettyDecl (DeclStruct s)  = prettyStruct s
prettyDecl (DeclVar v)     = prettyVarDecl v <> semi

prettyFunc :: FuncDecl -> Doc ann
prettyFunc f =
  kw "func"
    <+> pretty (funcName f)
    <> parens (hsep (punctuate comma (map prettyParam (funcParams f))))
    <> prettyReturn (funcRet f)
    <+> bracesBlock (map prettyStmt (funcBody f))

prettyParam :: Param -> Doc ann
prettyParam (Param name Nothing)  = pretty name
prettyParam (Param name (Just t)) = pretty name <+> kw ":" <+> prettyType t

prettyReturn :: Maybe Type -> Doc ann
prettyReturn Nothing  = mempty
prettyReturn (Just t) = space <> kw ":" <+> prettyType t

prettyStruct :: StructDecl -> Doc ann
prettyStruct s =
  kw "struct" <+> pretty (structName s)
  <+> bracesBlock (map prettyField (structFields s))

prettyField :: StructField -> Doc ann
prettyField (StructField name t) = pretty name <+> kw ":" <+> prettyType t <> semi

prettyStmt :: Stmt -> Doc ann
prettyStmt (StmtVar v)              = prettyVarDecl v <> semi
prettyStmt (StmtAssign lv e)        = prettyLValue lv <+> kw "=" <+> prettyExpr e <> semi
prettyStmt (StmtReturn Nothing)     = kw "return" <> semi
prettyStmt (StmtReturn (Just e))    = kw "return" <+> prettyExpr e <> semi
prettyStmt (StmtExpr e)             = prettyExpr e <> semi
prettyStmt (StmtInc lv)             = prettyLValue lv <> kw "++" <> semi
prettyStmt (StmtWhile c b)          =
  kw "while" <+> parens (prettyExpr c)
  <+> bracesBlock (map prettyStmt b)
prettyStmt (StmtIf c t e)           =
  kw "if" <+> parens (prettyExpr c)
  <+> bracesBlock (map prettyStmt t)
  <> maybe mempty (\b -> space <> kw "else" <+> bracesBlock (map prettyStmt b)) e
prettyStmt (StmtFor i c u b)        =
  kw "for"
    <+> parens
          ( prettyForInit i <> semi
            <+> prettyExpr c <> semi
            <+> prettyExpr u
          )
    <+> bracesBlock (map prettyStmt b)

prettyForInit :: Maybe AssignExpr -> Doc ann
prettyForInit Nothing                   = mempty
prettyForInit (Just (AssignExpr lv e))  = prettyLValue lv <+> kw "=" <+> prettyExpr e

prettyVarDecl :: VarDecl -> Doc ann
prettyVarDecl v =
  kw "let"
    <+> pretty (varName v)
    <> prettyVarType (varType v)
    <> prettyVarInit (varInit v)

prettyVarType :: Maybe Type -> Doc ann
prettyVarType Nothing  = mempty
prettyVarType (Just t) = space <> kw ":" <+> prettyType t

prettyVarInit :: Maybe Expr -> Doc ann
prettyVarInit Nothing  = mempty
prettyVarInit (Just e) = space <> kw "=" <+> prettyExpr e

prettyExpr :: Expr -> Doc ann
prettyExpr (EVar lv)            = prettyLValue lv
prettyExpr (ELit l)             = prettyLiteral l
prettyExpr (ECall f args)       = pretty f <> parens (hsep (punctuate comma (map prettyExpr args)))
prettyExpr (EAdd APlus a b)     = prettyExpr a <+> kw "+" <+> prettyExpr b
prettyExpr (EAdd AMinus a b)    = prettyExpr a <+> kw "-" <+> prettyExpr b
prettyExpr (EMul AMul a b)      = prettyExpr a <+> kw "*" <+> prettyExpr b
prettyExpr (EMul ADiv a b)      = prettyExpr a <+> kw "/" <+> prettyExpr b
prettyExpr (ERel op a b)        = prettyExpr a <+> prettyRel op <+> prettyExpr b
prettyExpr (EAnd a b)           = prettyExpr a <+> kw "&&" <+> prettyExpr b
prettyExpr (EOr a b)            = prettyExpr a <+> kw "||" <+> prettyExpr b
prettyExpr (ENew t e)           = kw "new" <+> prettyType t <> brackets (prettyExpr e)
prettyExpr (EParen e)           = parens (prettyExpr e)
prettyExpr (EArrayLit es)       = brackets (hsep (punctuate comma (map prettyExpr es)))
prettyExpr (EStructInit s es)   = pretty s <> braces (hsep (punctuate comma (map prettyExpr es)))
prettyExpr (EArraySize e)       = prettyExpr e <> kw ".size"

prettyLValue :: LValue -> Doc ann
prettyLValue (LVar s)       = pretty s
prettyLValue (LField lv f)  = prettyLValue lv <> kw "." <> pretty f
prettyLValue (LIndex lv e)  = prettyLValue lv <> brackets (prettyExpr e)

prettyType :: Type -> Doc ann
prettyType TInt                 = kw "int"
prettyType TFloat               = kw "float"
prettyType TBool                = kw "bool"
prettyType TString              = kw "string"
prettyType TVoid                = kw "void"
prettyType (TNamed s)           = pretty s
prettyType (TArray t Nothing)   = prettyType t <> kw "[]"
prettyType (TArray t (Just n))  = prettyType t <> brackets (pretty n)
prettyType (TFun a b)           = parens (prettyType a) <+> kw "->" <+> prettyType b

prettyLiteral :: Literal -> Doc ann
prettyLiteral (LInt i)     = pretty i
prettyLiteral (LFloat f)   = pretty f
prettyLiteral (LBool b)    = pretty b
prettyLiteral (LString s)  = pretty (show s)

prettyRel :: RelOp -> Doc ann
prettyRel AEq = kw "=="
prettyRel ANe = kw "!="
prettyRel ALt = kw "<"
prettyRel AGt = kw ">"
prettyRel ALe = kw "<="
prettyRel AGe = kw ">="
