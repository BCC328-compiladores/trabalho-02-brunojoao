module Errors.Diagnostic
  ( Severity(..)
  , Diagnostic(..)
  , Diagnostics
  , mkError
  ) where

import Token.Token (Position)

data Severity
  = Error
  | Warning
  deriving (Eq, Show)

data Diagnostic = Diagnostic
  { diagCode     :: String
  , diagSeverity :: Severity
  , diagPos      :: Maybe Position
  , diagMessage  :: String
  , diagHint     :: Maybe String
  }
  deriving (Eq, Show)

type Diagnostics = [Diagnostic]

mkError :: String -> String -> Diagnostic
mkError code msg =
  Diagnostic
    { diagCode = code
    , diagSeverity = Error
    , diagPos = Nothing
    , diagMessage = msg
    , diagHint = Nothing
    }