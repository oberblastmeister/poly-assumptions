module Syntax.Token where

import Protolude

data Token
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
  deriving (Show, Eq)
