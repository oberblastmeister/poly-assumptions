module Syntax.Expr
  ( Expr (..),
    Lit (..),
    BinOp (..),
    RecKind (..),
    everywhere,
  )
where

import Data.Text (Text)
import Pretty
import Prettyprinter (Doc, Pretty, pretty, (<+>))
import qualified Prettyprinter as P

type Name = Text

data RecKind = NonRec | Rec
  deriving (Show, Eq)

instance Pretty RecKind where
  pretty NonRec = ""
  pretty Rec = "rec"

data Expr
  = Lam Name Expr
  | Let RecKind Name Expr Expr
  | Fix Expr
  | App Expr Expr
  | Var Name
  | Lit Lit
  | Bin Expr BinOp Expr
  | If Expr Expr Expr
  deriving (Show, Eq)

everywhere :: (Expr -> Expr) -> Expr -> Expr
everywhere f = go
  where
    go = \case
      Lam x e -> f $ Lam x (go e)
      Let rk x e1 e2 -> f $ Let rk x (go e1) (go e2)
      App e1 e2 -> f $ App (go e1) (go e2)
      Bin e1 op e2 -> f $ Bin (go e1) op (go e2)
      If e1 e2 e3 -> f $ If (go e1) (go e2) (go e3)
      other -> f other

prettyWith :: Bool -> Expr -> Doc ann
prettyWith nest = \case
  Lam x e -> parensIf nest ("\\" <> pretty x <+> "->" <+> pretty e)
  Let rk x e1 e2 ->
    P.hsep
      [ "let",
        pretty rk,
        pretty x,
        "=",
        pretty e1,
        "in",
        pretty e2
      ]
  App e1 e2 -> prettyWith True e1 <+> prettyWith True e2
  Var x -> pretty x
  Lit l -> pretty l
  Bin e1 op e2 -> pretty e1 <+> pretty op <+> pretty e2
  If cond e1 e2 -> "if" <+> pretty cond <+> "then" <+> pretty e1 <+> "else" <+> pretty e2
  Fix e -> "fix" <+> pretty e

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