module Syntax.Token (Token (..)) where

import Data.Span
import Syntax.Token.Kind (TokenKind)

data Token = Token {kind :: TokenKind, span :: Span}
  deriving (Show, Eq)