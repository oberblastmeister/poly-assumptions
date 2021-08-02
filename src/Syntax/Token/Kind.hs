module Syntax.Token.Kind (TokenKind (..)) where

import Data.Text (Text)

data TokenKind
  = Let
  | Rec
  | Forall
  | True
  | False
  | In
  | If
  | Else
  | Then
  | Lambda
  | Num Int
  | Str Text
  | Ident Text
  | ConIdent Text
  | Arrow
  | Assign
  | Add
  | Sub
  | Mul
  | Div
  | Eq
  | NEq
  | LParen
  | RParen
  | Dot
  | Error
  | EOF
  deriving (Show, Eq)