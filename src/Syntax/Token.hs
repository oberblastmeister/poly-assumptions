module Syntax.Token where

import Data.Text (Text)

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
