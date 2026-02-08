{
module Parser.Parser where

import Control.Exception (Exception, throw)
import AST.AST
import Token.Token
}

%name parseProgram
%tokentype { Token }
%error { parseError }

%left Or
%left And
%nonassoc Equal NotEqual Less Greater LessEqual GreaterEqual
%nonassoc RBracket Semicolon RParenthesis
%left Plus Minus
%left Multiply Divide
%right Inc

%token
  -- types
  TypeInt      { TypeInt _ }
  TypeFloat    { TypeFloat _ }
  TypeBool     { TypeBool _ }
  TypeString   { TypeString _ }
  TypeVoid     { TypeVoid _ }

  -- identifiers / literals
  Identifier   { Identifier _ $$ }
  ValueInt     { ValueInt _ $$ }
  ValueFloat   { ValueFloat _ $$ }
  ValueString  { ValueString _ $$ }
  ValueBool    { ValueBool _ $$ }

  -- keywords
  Let          { Let _ }
  Return       { Return _ }
  While        { While _ }
  For          { For _ }
  If           { If _ }
  Else         { Else _ }
  Func         { Func _ }
  Struct       { Struct _ }
  New          { New _ }
  Forall       { Forall _ }

  -- operators
  Plus         { Plus _ }
  Minus        { Minus _ }
  Multiply     { Multiply _ }
  Divide       { Divide _ }
  Assign       { Assign _ }
  Equal        { Equal _ }
  NotEqual     { NotEqual _ }
  Less         { Less _ }
  Greater      { Greater _ }
  LessEqual    { LessEqual _ }
  GreaterEqual { GreaterEqual _ }
  And          { And _ }
  Or           { Or _ }
  Inc          { Inc _ }

  -- symbols
  Comma        { Comma _ }
  Semicolon    { Semicolon _ }
  Dot          { Dot _ }
  LParenthesis { LParenthesis _ }
  RParenthesis { RParenthesis _ }
  LBrace       { LBrace _ }
  RBrace       { RBrace _ }
  LBracket     { LBracket _ }
  RBracket     { RBracket _ }
  TypeDef      { TypeDef _ }
  Arrow        { Arrow _ }

%%

Program
  : DeclList                         { Program $1 }

DeclList
  : Decl DeclList                    { $1 : $2 }
  |                                  { [] }

Decl
  : FuncDecl                         { DeclFunc $1 }
  | StructDecl                       { DeclStruct $1 }
  | VarDecl                          { DeclVar $1 }

FuncDecl
  : ForallOpt Func Identifier LParenthesis ParamsOpt RParenthesis RetTypeOpt Block { FuncDecl $1 $3 $5 $7 $8 }

ForallOpt
  : Forall TypeParams Dot { $2 }
  |                        { [] }

TypeParams
  : Identifier            { [$1] }
  | Identifier TypeParams { $1 : $2 }

RetTypeOpt
  : TypeDef Type                     { Just $2 }
  |                                  { Nothing }

ParamsOpt
  : Params                           { $1 }
  |                                  { [] }

Params
  : Param                 { [$1] }
  | Param Comma Params    { $1 : $3 }

Param
  : Identifier ParamTypeOpt { Param $1 $2 }

ParamTypeOpt
  : TypeDef Type { Just $2 }
  |              { Nothing }

StructDecl
  : Struct Identifier LBrace Fields RBrace { StructDecl $2 $4 }

Fields
  : Field Fields                     { $1 : $2 }
  |                                  { [] }

Field
  : Identifier TypeDef Type Semicolon { StructField $1 $3 }

VarDecl
  : Let Identifier TypeDef Type InitOpt Semicolon { VarDecl $2 (Just $4) $5 }
  | Let Identifier Assign Expr Semicolon          { VarDecl $2 Nothing (Just $4) }

InitOpt
  : Assign Expr                      { Just $2 }
  |                                  { Nothing }

Block
  : LBrace Stmts RBrace              { $2 }

Stmts
  : Stmt Stmts                       { $1 : $2 }
  |                                  { [] }

Stmt
  : VarDecl                                                                           { StmtVar $1 }
  | LValue Assign Expr Semicolon                                                      { StmtAssign $1 $3 }
  | If LParenthesis Expr RParenthesis Block ElseOpt                                   { StmtIf $3 $5 $6 }
  | While LParenthesis Expr RParenthesis Block                                        { StmtWhile $3 $5 }
  | For LParenthesis ForInitOpt Semicolon Expr Semicolon ForUpdate RParenthesis Block { StmtFor $3 $5 $7 $9 }
  | Return ReturnOpt Semicolon                                                        { StmtReturn $2 }
  | Expr Semicolon                                                                    { StmtExpr $1 }
  | LValue Inc Semicolon                                                              { StmtInc $1 }

ForUpdate
  : LValue Inc   { EPostInc $1 }
  | Expr         { $1 }

ElseOpt
  : Else Block                       { Just $2 }
  |                                  { Nothing }

ForInitOpt
  : LValue Assign Expr               { Just (AssignExpr $1 $3) }
  |                                  { Nothing }

ReturnOpt
  : Expr                             { Just $1 }
  |                                  { Nothing }

Expr
  : Expr Or AndExpr                  { EOr $1 $3 }
  | AndExpr                          { $1 }

AndExpr
  : AndExpr And RelExpr              { EAnd $1 $3 }
  | RelExpr                          { $1 }

RelExpr
  : RelExpr RelOp AddExpr            { ERel $2 $1 $3 }
  | AddExpr                          { $1 }

RelOp
  : Equal        { AEq }
  | NotEqual     { ANe }
  | Less         { ALt }
  | Greater      { AGt }
  | LessEqual    { ALe }
  | GreaterEqual { AGe }

AddExpr
  : AddExpr Plus Term                { EAdd APlus $1 $3 }
  | AddExpr Minus Term               { EAdd AMinus $1 $3 }
  | Term                             { $1 }

Term
  : Term Multiply Primary     { EMul AMul $1 $3 }
  | Term Divide Primary       { EMul ADiv $1 $3 }
  | Primary                   { $1 }

Primary
  : LParenthesis Expr RParenthesis               { EParen $2 }
  | Identifier LParenthesis ArgsOpt RParenthesis { ECall $1 $3 }
  | LValue                                       { lvalueToExpr $1 }
  | Literal                                      { ELit $1 }
  | LBracket ArgsOpt RBracket                    { EArrayLit $2 }
  | Identifier LBrace ArgsOpt RBrace             { EStructInit $1 $3 }
  | New BaseType LBracket NewSize RBracket       { ENew $2 $4 }

NewSize
  : Expr { $1 }

ArgsOpt
  : Args                             { $1 }
  |                                  { [] }

Args
  : Expr                             { [$1] }
  | Expr Comma Args                  { $1 : $3 }

LValue
  : Identifier                       { LVar $1 }
  | LValue Dot Identifier            { LField $1 $3 }
  | LValue LBracket Expr RBracket    { LIndex $1 $3 }

Literal
  : ValueInt                         { LInt $1 }
  | ValueFloat                       { LFloat $1 }
  | ValueBool                        { LBool $1 }
  | ValueString                      { LString $1 }

Type
  : FuncType { $1 }

FuncType
  : TypeBase Arrow Type   { TFun $1 $3 }
  | TypeBase              { $1 }

TypeBase
  : BaseType                            { $1 }
  | TypeBase LBracket RBracket          { TArray $1 Nothing }
  | TypeBase LBracket ValueInt RBracket { TArray $1 (Just $3) }

BaseType
  : TypeInt      { TInt }
  | TypeFloat    { TFloat }
  | TypeBool     { TBool }
  | TypeString   { TString }
  | TypeVoid     { TVoid }
  | Identifier   { TNamed $1 }
  | LParenthesis Type RParenthesis { $2 }

{
data ParserException
  = ParserException [Token]

instance Exception ParserException

instance Show ParserException where
  show (ParserException []) =
    "Parse error (unexpected end of input)"

  show (ParserException (tok:_)) =
    let Position l c = tokenPos tok
    in  "Parse error at line " ++ show l ++
        ", column " ++ show c ++
        "\nUnexpected token: " ++ tokenShort tok

tokenShort :: Token -> String
tokenShort (Identifier _ s)   = "Identifier \"" ++ s ++ "\""
tokenShort (ValueInt _ i)     = "Int " ++ show i
tokenShort (ValueFloat _ f)   = "Float " ++ show f
tokenShort (ValueString _ s)  = "String " ++ show s
tokenShort (ValueBool _ b)    = show b
tokenShort t                  = takeWhile (/= ' ') (show t)

parseError :: [Token] -> a
parseError toks =
  throw (ParserException toks)

lvalueToExpr :: LValue -> Expr
lvalueToExpr lv =
  case lv of
    LField base "size" -> EArraySize (EVar base)
    _ -> EVar lv
}