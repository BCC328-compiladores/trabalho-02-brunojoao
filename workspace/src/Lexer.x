{

module Lexer where

import Token
import Control.Exception

}

%wrapper "posn"

$digit  = 0-9
$alpha  = [a-zA-Z]
$idchar = [$alpha $digit _]

tokens :-

    $white+                         ;
    "//".*                          ;

    -- literals
    \"[^\"]*\"                      { \pos s -> ValueString (posToPosition pos) (init (tail s)) }
    $digit+ "." $digit+             { \pos s -> ValueFloat  (posToPosition pos) (read s) }
    $digit+                         { \pos s -> ValueInt    (posToPosition pos) (read s) }
    true                            { \pos _ -> ValueBool   (posToPosition pos) True }
    false                           { \pos _ -> ValueBool   (posToPosition pos) False }

    -- keywords
    let                             { \pos _ -> Let     (posToPosition pos) }
    return                          { \pos _ -> Return  (posToPosition pos) }
    int                             { \pos _ -> TypeInt (posToPosition pos) }
    string                          { \pos _ -> TypeString (posToPosition pos) }
    float                           { \pos _ -> TypeFloat  (posToPosition pos) }
    void                            { \pos _ -> TypeVoid   (posToPosition pos) }
    bool                            { \pos _ -> TypeBool   (posToPosition pos) }
    while                           { \pos _ -> While  (posToPosition pos) }
    for                             { \pos _ -> For    (posToPosition pos) }
    if                              { \pos _ -> If     (posToPosition pos) }
    else                            { \pos _ -> Else   (posToPosition pos) }
    func                            { \pos _ -> Func   (posToPosition pos) }
    struct                          { \pos _ -> Struct (posToPosition pos) }
    new                             { \pos _ -> New    (posToPosition pos) }
    forall                          { \pos _ -> Forall (posToPosition pos) }

    -- operators
    "=="                            { \pos _ -> Equal        (posToPosition pos) }
    "!="                            { \pos _ -> NotEqual     (posToPosition pos) }
    "<="                            { \pos _ -> LessEqual    (posToPosition pos) }
    ">="                            { \pos _ -> GreaterEqual (posToPosition pos) }
    "&&"                            { \pos _ -> And          (posToPosition pos) }
    "||"                            { \pos _ -> Or           (posToPosition pos) }
    "->"                            { \pos _ -> Arrow        (posToPosition pos) }
    "++"                            { \pos _ -> Inc          (posToPosition pos) }

    -- single character operators
    "-"                             { \pos _ -> Minus    (posToPosition pos) }
    "+"                             { \pos _ -> Plus     (posToPosition pos) }
    "*"                             { \pos _ -> Multiply (posToPosition pos) }
    "/"                             { \pos _ -> Divide   (posToPosition pos) }
    "="                             { \pos _ -> Assign   (posToPosition pos) }
    "<"                             { \pos _ -> Less     (posToPosition pos) }
    ">"                             { \pos _ -> Greater  (posToPosition pos) }

    -- symbols
    "("                             { \pos _ -> LParenthesis (posToPosition pos) }
    ")"                             { \pos _ -> RParenthesis (posToPosition pos) }
    "{"                             { \pos _ -> LBrace       (posToPosition pos) }
    "}"                             { \pos _ -> RBrace       (posToPosition pos) }
    "["                             { \pos _ -> LBracket     (posToPosition pos) }
    "]"                             { \pos _ -> RBracket     (posToPosition pos) }
    ":"                             { \pos _ -> TypeDef      (posToPosition pos) }
    ","                             { \pos _ -> Comma        (posToPosition pos) }
    ";"                             { \pos _ -> Semicolon    (posToPosition pos) }
    "."                             { \pos _ -> Dot          (posToPosition pos) }

    -- identifier
    $alpha $idchar*                 { \pos s -> Identifier (posToPosition pos) s }

    -- lexer error
    .                               { \pos s -> lexerError pos s }

{

posToPosition :: AlexPosn -> Position
posToPosition (AlexPn _ line col) = Position line col

data LexerException
    = LexerException Position String

instance Show LexerException where
  show (LexerException (Position l c) s) =
    "Lexical error at line " ++ show l ++
    ", column " ++ show c ++
    ", invalid character: " ++ show s
    
instance Exception LexerException

lexerError :: AlexPosn -> String -> a
lexerError pos s =
    throw (LexerException (posToPosition pos) s)

}
