module Syntax.Token where

import Data.Span
import Data.Text (Text)
import Syntax.Token.Kind (TokenKind)

data Token = Token {kind :: TokenKind, span :: Span}
  deriving (Show, Eq)