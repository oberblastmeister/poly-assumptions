module Types.Infer where

import Control.Arrow (second)
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Supply
import qualified Data.DList as DL
import qualified Data.EnumSet as ESet
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.HashSet as HSet
import Debugging
import Lens.Micro ((%~))
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
  let !_ = dbg $ "inferring: " ++ show ex
  (t, st) <- infer ex & (`runStateT` defInferState)
  let !_ = dbg @String "resulting state"
  let !_ = dbg st
  let !_ = dbg @String "inferred type"
  let !_ = dbgP' T.prettyRaw t
  let unbounds = As.keys $ _assumptions st
  unless (HSet.null unbounds) $ throwError $ UnboundVariable (HSet.toList unbounds |> head)
  let !_ = dbg' "constraints" $ DL.toList $ _constraints st
  subst <- Solve.solve $ DL.toList $ _constraints st
  let !_ = dbgP subst
  let res = (subst, subst @@ t)
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
infer expr = do
  curr <- get
  let !_ = dbg $ "current state: " ++ show curr
  case expr of
    Expr.Lit l -> return $ litTy l
    Expr.Var x -> do
      tv <- freshT
      let !_ = dbg $ "added assumption " ++ show x ++ " -> " ++ show tv
      modify $ addAssumption (x, tv)
      return tv
    Expr.Lam x e -> do
      v <- freshV
      let !_ = dbg $ "freshV: " ++ show v
      let tv = T.Var v
      (t, st) <- localState (boundVars %~ ESet.insert v) (takeState $ infer e)
      let !_ = dbg $ "inferred lam body: " ++ show t
      removeAssumptions x st
        & (`addConstraints` [ConEqual t' tv | t' <- lookupAssumptions x st])
        & put
      return $ tv T.:-> t
    Expr.App e1 e2 -> do
      t1 <- infer e1
      t2 <- infer e2
      tv <- freshT
      modify $ addConstraint $ ConEqual t1 (t2 T.:-> tv)
      let !_ = dbg $ "inferred app: " ++ show tv
      return tv
    Expr.Let _rk x e1 e2 -> do
      t1 <- infer e1
      let !_ = dbg $ "in let: inferred val: " ++ show t1
      (t2, st) <- infer e2 & takeState
      let !_ = dbg $ "in let: inferred body: " ++ show t2
      vs <- gets _boundVars
      let vsSet = ESet.toList vs <&> T.Var & HSet.fromList
      removeAssumptions x st
        & (`addConstraints` [ConImplicit t' vsSet t1 | t' <- lookupAssumptions x st])
        & put
      return t2
    Expr.Bin e1 op e2 -> do
      t1 <- infer e1
      t2 <- infer e2
      tv <- freshT
      let u1 = t1 T.:-> t2 T.:-> tv
          u2 = ops op
      modify $ addConstraint $ ConEqual u1 u2
      return tv
    Expr.If cond e1 e2 -> do
      t1 <- infer cond
      modify $ addConstraint $ ConEqual t1 T.bool
      t2 <- infer e1
      t3 <- infer e2
      modify $ addConstraint $ ConEqual t2 t3
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
