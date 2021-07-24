module Syntax.Expr
  ( Expr (..),
    Lit (..),
    BinOp (..),
  )
where

import Pretty
import Prettyprinter (Doc, Pretty, pretty, (<+>))
import Protolude

type Name = Text

data Expr
  = Lam Name Expr
  | Let Name Expr Expr
  | App Expr Expr
  | Var Name
  | Lit Lit
  | Bin Expr BinOp Expr
  | If Expr Expr Expr
  deriving (Show, Eq)

prettyWith :: Bool -> Expr -> Doc ann
prettyWith nest = \case
  Lam x e -> parensIf nest ("\\" <> pretty x <+> "->" <+> pretty e)
  Let x e1 e2 -> "let" <+> pretty x <+> "=" <+> pretty e1 <+> "in" <+> pretty e2
  App e1 e2 -> prettyWith True e1 <+> prettyWith True e2
  Var x -> pretty x
  Lit l -> pretty l
  Bin e1 op e2 -> pretty e1 <+> pretty op <+> pretty e2
  If cond e1 e2 -> "if" <+> pretty cond <+> "then" <+> pretty e1 <+> "else" <+> pretty e2

instance Pretty Expr where
  pretty = prettyWith False

data BinOp
  = Add
  | Sub
  | Mul
  | Div
  | Eq
  | NEq
  deriving (Show, Eq)

instance Pretty BinOp where
  pretty = \case
    Add -> "+"
    Sub -> "-"
    Mul -> "*"
    Div -> "/"
    Eq -> "=="
    NEq -> "!="

data Lit
  = LInt Int
  | LBool Bool
  deriving (Show, Eq)

instance Pretty Lit where
  pretty (LInt i) = pretty i
  pretty (LBool b) = pretty b
