module Syntax.Token where

import Protolude

data Token
  = Let
  | Forall
  | True
  | False
  | In
  | Lambda
  | Num Int
  | Ident Text
  | ConIdent Text
  | Arrow
  | Eq
  | Add
  | Sub
  | Mul
  | Div
  | LParen
  | RParen
  | Dot
  deriving (Show, Eq)
