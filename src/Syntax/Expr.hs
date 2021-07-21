module Syntax.Expr where

import Data.Text (Text)

type Name = Text

data Expr
  = Lam Name Expr
  | App Expr Expr
  | Var Name
  | Lit Lit
  | Bin Expr BinOp Expr

data BinOp
  = Add
  | Sub
  | Mul
  | Div
  | Eq
  | NEq

data Lit
  = LInt Int
  | LBool Bool
