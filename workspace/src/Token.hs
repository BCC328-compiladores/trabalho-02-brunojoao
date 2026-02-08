module Token where

data Position = Position
  { posLine   :: !Int
  , posColumn :: !Int
  }
  deriving (Eq, Show)

data Token

    -- types
    = TypeInt       { tokenPos :: Position }
    | TypeString    { tokenPos :: Position }
    | TypeFloat     { tokenPos :: Position }
    | TypeBool      { tokenPos :: Position }
    | TypeVoid      { tokenPos :: Position }

    -- identifier
    | Identifier    { tokenPos :: Position, identName :: String }

    -- literals
    | ValueInt      { tokenPos :: Position, intVal   :: Int }
    | ValueFloat    { tokenPos :: Position, floatVal :: Double }
    | ValueString   { tokenPos :: Position, strVal   :: String }
    | ValueBool     { tokenPos :: Position, boolVal  :: Bool }

    -- keywords
    | Let           { tokenPos :: Position }
    | Return        { tokenPos :: Position }
    | While         { tokenPos :: Position }
    | For           { tokenPos :: Position }
    | If            { tokenPos :: Position }
    | Else          { tokenPos :: Position }
    | Func          { tokenPos :: Position }
    | Struct        { tokenPos :: Position }
    | Forall        { tokenPos :: Position }
    | New           { tokenPos :: Position }

    -- operators
    | Minus         { tokenPos :: Position }
    | Plus          { tokenPos :: Position }
    | Multiply      { tokenPos :: Position }
    | Divide        { tokenPos :: Position }
    | Assign        { tokenPos :: Position }
    | Equal         { tokenPos :: Position }
    | NotEqual      { tokenPos :: Position }
    | LessEqual     { tokenPos :: Position }
    | GreaterEqual  { tokenPos :: Position }
    | Less          { tokenPos :: Position }
    | Greater       { tokenPos :: Position }
    | And           { tokenPos :: Position }
    | Or            { tokenPos :: Position }
    | Arrow         { tokenPos :: Position }
    | Inc           { tokenPos :: Position }

    -- symbols
    | Comma         { tokenPos :: Position }
    | Semicolon     { tokenPos :: Position }
    | Dot           { tokenPos :: Position }
    | LParenthesis  { tokenPos :: Position }
    | RParenthesis  { tokenPos :: Position }
    | LBrace        { tokenPos :: Position }
    | RBrace        { tokenPos :: Position }
    | LBracket      { tokenPos :: Position }
    | RBracket      { tokenPos :: Position }
    | TypeDef       { tokenPos :: Position }

    deriving (Eq, Show)
