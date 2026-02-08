module ASTPretty (programTree) where

import AST
import Data.Tree (Tree(..))

programTree :: Program -> Tree String
programTree (Program ds) = Node "Program" (map declTree ds)

declTree :: Decl -> Tree String
declTree (DeclFunc f)   = funcTree f
declTree (DeclStruct s) = Node ("Struct " ++ structName s) (map fieldTree (structFields s))
declTree (DeclVar v)    = Node "GlobalVar" [varDeclTree v]

funcTree :: FuncDecl -> Tree String
funcTree (FuncDecl n ps r b) =
  Node ("Func " ++ n)
    [ Node "Params" (map paramTree ps)
    , Node "ReturnType" (maybe [] (\t -> [typeTree t]) r)
    , Node "Body" (map stmtTree b)
    ]

paramTree :: Param -> Tree String
paramTree (Param n t) = Node ("Param " ++ n) (maybe [] (\x -> [typeTree x]) t)

fieldTree :: StructField -> Tree String
fieldTree (StructField n t) = Node ("Field " ++ n) [typeTree t]

varDeclTree :: VarDecl -> Tree String
varDeclTree (VarDecl n t i) =
  Node ("Var " ++ n)
    ( maybe [] (\x -> [typeTree x]) t
   ++ maybe [] (\e -> [exprTree e]) i )

stmtTree :: Stmt -> Tree String
stmtTree (StmtVar v)        = Node "VarDecl" [varDeclTree v]
stmtTree (StmtAssign l e)   = Node "=" [lvalueTree l, exprTree e]
stmtTree (StmtInc lv)       = Node "++" [lvalueTree lv]
stmtTree (StmtReturn e)     = Node "Return" (maybe [] (\x -> [exprTree x]) e)
stmtTree (StmtExpr e)       = Node "Expr" [exprTree e]
stmtTree (StmtWhile c b)    = Node "While" [exprTree c, Node "Body" (map stmtTree b)]
stmtTree (StmtIf c t e)     =
    Node "If"
        ( exprTree c
        : Node "Then" (map stmtTree t)
        : maybe [] (\b -> [Node "Else" (map stmtTree b)]) e
        )
stmtTree (StmtFor i c u b)  =
    Node "For"
        [ Node "Init" (maybe [] assignTree i)
        , Node "Cond" [exprTree c]
        , Node "Update" [exprTree u]
        , Node "Body" (map stmtTree b)
        ]

assignTree :: AssignExpr -> [Tree String]
assignTree (AssignExpr l e) = [Node "=" [lvalueTree l, exprTree e]]

exprTree :: Expr -> Tree String
exprTree (EVar lv)          = lvalueTree lv
exprTree (ELit l)           = Node (show l) []
exprTree (EArraySize e)     = Node "size" [exprTree e]
exprTree (ECall f as)       = Node ("Call " ++ f) [Node "Args" (map exprTree as)]
exprTree (EAdd o a b)       = Node (show o) [exprTree a, exprTree b]
exprTree (EMul o a b)       = Node (show o) [exprTree a, exprTree b]
exprTree (ERel o a b)       = Node (show o) [exprTree a, exprTree b]
exprTree (EAnd a b)         = Node "&&" [exprTree a, exprTree b]
exprTree (EOr a b)          = Node "||" [exprTree a, exprTree b]
exprTree (ENew t e)         = Node "new" [typeTree t, exprTree e]
exprTree (EArrayLit es)     = Node "Array" (map exprTree es)
exprTree (EStructInit s es) = Node ("StructInit " ++ s) (map exprTree es)
exprTree (EParen e)         = exprTree e

lvalueTree :: LValue -> Tree String
lvalueTree (LVar s)     = Node s []
lvalueTree (LField l f) = Node ("." ++ f) [lvalueTree l]
lvalueTree (LIndex l e) = Node "[]" [lvalueTree l, exprTree e]

typeTree :: Type -> Tree String
typeTree t = Node (show t) []
