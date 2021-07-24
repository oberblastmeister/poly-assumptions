module Desugar where

import Syntax.Expr (Expr)
import qualified Syntax.Expr as Expr

desugarExpr :: Expr -> Expr
desugarExpr = Expr.everywhere $ \case
  Expr.Let Expr.Rec x e1 e2 ->
    Expr.Let Expr.NonRec x (Expr.Fix (Expr.Lam "x" e1)) e2
  other -> other
