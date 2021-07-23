module Types.Solve where

import Control.Monad.Except
import qualified Data.EnumSet as ESet
import Protolude
import Types.Infer.Monad (Constraint (..), TypeError)
import Types.Subst (Subst, Substitutable ((@@)))
import qualified Types.Subst as Subst
import Types.Unify (unify)

type MonadSolve m = (MonadError TypeError m)

solve :: MonadError TypeError m => [Constraint] -> m Subst
solve [] = return Subst.empty
solve cs = solve' $ nextSolvable cs

solve' :: MonadError TypeError m => (Constraint, [Constraint]) -> m Subst
solve' (ConEqual t1 t2, cs) = do
  su1 <- unify t1 t2
  su2 <- solve (su1 @@ cs)
  return $ su2 <> su1
solve' (ConImplicit t1 ms t2, cs) =
  solve (ConExplicit t1 (generalize ms t2) : cs)
solve' (ConExplicit t s, cs) = do
  s' <- instantiate s
  solve (ConEqual t s' : cs)

instantiate = undefined

generalize = undefined

nextSolvable :: [Constraint] -> (Constraint, [Constraint])
nextSolvable xs = fromJust (find solvable chosen)
  where
    chosen = [(x, ys) | x <- xs, let ys = delete x xs]

solvable :: (Constraint, [Constraint]) -> Bool
solvable (ConEqual {}, _) = True
solvable (ConExplicit {}, _) = True
solvable (c@(ConImplicit _t1 ms t2), cs) = error $ "uhoh, constraint `" ++ show c ++ "` wasn't solvable"
