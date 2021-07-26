module Types.Solve where

import Control.Monad.Except
import Control.Monad.Supply
import Data.EnumSet (EnumSet)
import qualified Data.EnumSet as ESet
import Protolude
import Types.Infer.Monad (Constraint (..), TypeError, freshT)
import Types.Subst (Subst, Substitutable ((@@)))
import qualified Types.Subst as Subst
import Types.Type (Type)
import qualified Types.Type as T
import Types.Unify (unify)
import Debugging

type MonadSolve m = (MonadError TypeError m)

solve :: (MonadError TypeError m, MonadSupply T.Var m) => [Constraint] -> m Subst
solve [] = return Subst.empty
solve cs = solve' $ nextSolvable cs

solve' :: (MonadError TypeError m, MonadSupply T.Var m) => (Constraint, [Constraint]) -> m Subst
solve' a | dbg' ("solving " ++ show a) False = undefined
solve' (ConEqual t1 t2, cs) = do
  let !_ = dbg $ "cs: " ++ show cs
  su1 <- unify t1 t2
  let !_ = dbg $ "su1: " ++ show su1
  su2 <- solve (su1 @@ cs)
  return $ su2 <> su1
solve' (ConImplicit t1 ms t2, cs) =
  solve (ConExplicit t1 (dbg' "generalized type" (generalize ms t2)) : cs)
solve' (ConExplicit t s, cs) = do
  s' <- dbg' "instantiated type" <$> instantiate s
  solve (dbg' "solving after instantiate" (ConEqual t s' : cs))

instantiate :: MonadSupply T.Var m => T.Scheme -> m Type
instantiate (T.Forall as t) = do
  as' <- traverse (const freshT) as
  let s = Subst.fromList $ zip as as'
  return $ s @@ t

generalize :: EnumSet T.Var -> Type -> T.Scheme
generalize free t = T.Forall as t
  where
    as = ESet.toList $ Subst.ftv t `ESet.difference` free

nextSolvable :: [Constraint] -> (Constraint, [Constraint])
nextSolvable xs = fromJust (find solvable $ chooseOne xs)
  where
    chooseOne xs' = [(x, ys) | x <- xs, let ys = delete x xs']

solvable :: (Constraint, [Constraint]) -> Bool
solvable (ConEqual {}, _) = True
solvable (ConExplicit {}, _) = True
solvable (c@(ConImplicit _t1 ms t2), cs) =
  let res = ESet.null
          ( (Subst.ftv t2 `ESet.difference` ms)
              `ESet.intersection` Subst.atv cs
          )
    in
    let !_ = dbg $ "con implicit solvable? " ++ show res
     in res
    -- || error ("uhoh, constraint `" ++ show c ++ "` wasn't solvable")
