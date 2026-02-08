module AST where

-- Programa
data Program
  = Program [Decl]
  deriving (Show, Eq)

-- Declarações
data Decl
  = DeclFunc FuncDecl
  | DeclStruct StructDecl
  | DeclVar VarDecl
  deriving (Show, Eq)

-- Funções
data FuncDecl = FuncDecl
  { funcName   :: String
  , funcParams :: [Param]
  , funcRet    :: Maybe Type
  , funcBody   :: Block
  }
  deriving (Show, Eq)

data Param = Param String (Maybe Type)
  deriving (Show, Eq)

-- Structs
data StructDecl = StructDecl
  { structName   :: String
  , structFields :: [StructField]
  }
  deriving (Show, Eq)

data StructField = StructField String Type
  deriving (Show, Eq)

-- Variáveis
data VarDecl = VarDecl
  { varName :: String
  , varType :: Maybe Type   
  , varInit :: Maybe Expr
  }
  deriving (Show, Eq)

-- Blocos e Statements
type Block = [Stmt]

data Stmt
  = StmtVar VarDecl
  | StmtAssign LValue Expr
  | StmtIf Expr Block (Maybe Block)
  | StmtWhile Expr Block
  | StmtFor (Maybe AssignExpr) Expr Expr Block
  | StmtReturn (Maybe Expr)
  | StmtExpr Expr
  | StmtInc LValue
  deriving (Show, Eq)

-- Atribuição sem ';'
data AssignExpr = AssignExpr LValue Expr
  deriving (Show, Eq)

-- LValue
data LValue
  = LVar String
  | LField LValue String
  | LIndex LValue Expr
  deriving (Show, Eq)

-- Expressões
data Expr
  = EOr Expr Expr
  | EAnd Expr Expr
  | ERel RelOp Expr Expr
  | EAdd AddOp Expr Expr
  | EMul MulOp Expr Expr
  | ECall String [Expr]
  | EVar LValue
  | ELit Literal
  | EArrayLit [Expr]
  | EStructInit String [Expr]
  | ENew Type Expr
  | EParen Expr
  | EArraySize Expr  
  deriving (Show, Eq)

data RelOp = AEq | ANe | ALt | AGt | ALe | AGe
  deriving (Show, Eq)

data AddOp = APlus | AMinus
  deriving (Show, Eq)

data MulOp = AMul | ADiv
  deriving (Show, Eq)

-- Tipos
data Type
  = TInt
  | TFloat
  | TBool
  | TString
  | TVoid
  | TArray Type (Maybe Int)
  | TNamed String
  | TFun Type Type
  deriving (Show, Eq)

-- Literais
data Literal
  = LInt Int
  | LFloat Double
  | LBool Bool
  | LString String
  deriving (Show, Eq)
