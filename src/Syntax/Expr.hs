module Syntax.Expr
  ( Expr (..),
    Lit (..),
    BinOp(..)
  )
where

import Data.Text (Text)

type Name = Text

data Expr
  = Lam Name Expr
  | Let Name Expr Expr
  | App Expr Expr
  | Var Name
  | Lit Lit
  | Bin Expr BinOp Expr
  | If Expr Expr Expr

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
