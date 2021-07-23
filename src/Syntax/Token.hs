module Syntax.Token where

import Protolude

data Token
  = Let
  | True
  | False
  | In
  | Lambda
  | Num Int
  | Ident Text
  | Arrow
  | Eq
  | Add
  | Sub
  | Mul
  | Div
  | LParen
  | RParen
  deriving (Show, Eq)
