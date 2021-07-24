module Types.Infer where

import Control.Arrow (second)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Supply
import qualified Data.DList as DL
import qualified Data.EnumSet as ESet
import qualified Data.EnumSet as EnumSet
import Data.Foldable (toList)
import Data.Function ((&))
import qualified Data.HashSet as HSet
import Debugging
import Protolude
import Syntax.Expr (Expr)
import qualified Syntax.Expr as Expr
import qualified Types.Assumptions as As
import Types.Infer.Monad
import qualified Types.Solve as Solve
import Types.Subst (Subst, Substitutable ((@@)))
import qualified Types.Subst as Subst
import Types.Type (Type)
import qualified Types.Type as T

inferExpr :: (MonadError TypeError m, MonadSupply T.Var m) => Expr -> m T.Scheme
inferExpr = inferType >=> (\(_, t) -> return $ closeOver t)

inferType :: (MonadError TypeError m, MonadSupply T.Var m) => Expr -> m (Subst, Type)
inferType ex = do
  (t, st) <- infer ex & (`runReaderT` EnumSet.empty) & (`runStateT` defInferState)
  let unbounds = As.keys $ _assumptions st
  unless (HSet.null unbounds) $ throwError $ UnboundVariable (HSet.toList unbounds |> head)
  subst <- Solve.solve (dbg' "constraints" $ DL.toList $ _constraints st)
  let !_ = dbg subst
  let res = dbg (subst, subst @@ t)
  return res

closeOver :: Type -> T.Scheme
closeOver = normalize . Solve.generalize ESet.empty

normalize :: T.Scheme -> T.Scheme
normalize (T.Forall as t) = T.Forall as' (s @@ t)
  where
    zipped = zip as varSupply
    as' = snd <$> zipped
    s = Subst.fromList (second T.Var <$> zipped)

-- seems to assume everything is alpha renamed probably
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
    (t, st) <- infer e & withMVar v & takeState
    removeAssumptions st x
      & (`addConstraints` [ConEqual t' tv | t' <- toList $ As.lookup x $ _assumptions st])
      & put
    return $ tv T.:-> t
  Expr.App e1 e2 -> do
    t1 <- infer e1
    t2 <- infer e2
    tv <- freshT
    addConstraint $ ConEqual t1 (t2 T.:-> tv)
    return tv
  Expr.Let _rk x e1 e2 -> do
    t1 <- infer e1
    (t2, st) <- infer e2 & listenState
    ms <- ask
    removeAssumptions st x
      & (`addConstraints` [ConImplicit t' ms t1 | t' <- toList $ As.lookup x $ _assumptions st])
      & put
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
  _ -> error "nyi"

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
