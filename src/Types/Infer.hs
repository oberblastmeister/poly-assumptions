module Types.Infer where

import Data.Foldable (toList)
import Syntax.Expr (Expr)
import qualified Syntax.Expr as Expr
import Types.Infer.Monad
import Types.Type (Type)
import qualified Types.Type as T

infer :: MonadInfer m => Expr -> m Type
infer expr = case expr of
  Expr.Lit l -> return $ litTy l
  Expr.Var x -> do
    tv <- freshT
    addAssumption (x, tv)
    return tv
  Expr.Lam x e -> do
    v <- freshV
    let tv = T.Var v
    t <- withMVar v (infer e)
    removeAssumptions x
    ts <- toList <$> lookupAssumptions x
    addConstraints [ConEqual t' tv | t' <- ts]
    return t
  Expr.App e1 e2 -> do
    t1 <- infer e1
    t2 <- infer e2
    tv <- freshT
    addConstraint $ ConEqual t1 (t2 T.:-> tv)
    return tv
  Expr.Let x e1 e2 -> do
    t1 <- infer e1
    t2 <- infer e2
    ms <- getMSet
    ts <- toList <$> lookupAssumptions x
    addConstraints $ [ConImplInst t' ms t1 | t' <- ts]
    return t2
  Expr.Bin e1 op e2 -> do
    t1 <- infer e1
    t2 <- infer e2
    tv <- freshT
    let u1 = t1 T.:-> t2 T.:-> tv
        u2 = ops op
    addConstraint $ ConEqual u1 u2
    return tv
  Expr.If cond e1 e2 -> do
    t1 <- infer cond
    addConstraint $ ConEqual t1 T.bool
    t2 <- infer e1
    t3 <- infer e2
    addConstraint $ ConEqual t2 t3
    return t2

litTy :: Expr.Lit -> Type
litTy = \case
  Expr.LInt _ -> T.int
  Expr.LBool _ -> T.bool

ops :: Expr.BinOp -> Type
ops Expr.Add = T.int T.:-> T.int T.:-> T.int
ops Expr.Mul = T.int T.:-> T.int T.:-> T.int
ops Expr.Div = T.int T.:-> T.int T.:-> T.int
ops Expr.Sub = T.int T.:-> T.int T.:-> T.int
ops Expr.Eq = T.int T.:-> T.int T.:-> T.bool
ops Expr.NEq = T.int T.:-> T.int T.:-> T.bool
