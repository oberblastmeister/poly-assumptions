module Types.Unify where

import Control.Monad.Except (throwError)
import qualified Data.EnumSet as EnumSet
import Types.Infer.Monad
import Types.Subst (Subst)
import qualified Types.Subst as Subst
import Types.Type (Type)
import qualified Types.Type as T

unify :: MonadInfer m => Type -> Type -> m Subst
unify t1 t2 | t1 == t2 = return Subst.empty
unify (T.Var v) t = v `bind` t
unify t (T.Var v) = v `bind` t
unify (t1 T.:-> t2) (t3 T.:-> t4) = unifyMany [t1, t2] [t3, t4]
unify t1 t2 = throwError $ TypeMismatch t1 t2

unifyMany :: MonadInfer m => [Type] -> [Type] -> m Subst
unifyMany [] [] = return Subst.empty
unifyMany (t1 : ts1) (t2 : ts2) = do
  su1 <- unify t1 t2
  su2 <- unifyMany (su1 Subst.@@ ts1) (su1 Subst.@@ ts2)
  return (su2 <> su1)
unifyMany t1 t2 = throwError $ TypeMismatchMulti t1 t2

bind :: MonadInfer m => T.Var -> Type -> m Subst
bind v t
  | t == T.Var v = return Subst.empty
  | occursCheck v t = throwError $ InfiniteType v t
  | otherwise = return $ Subst.singleton v t

occursCheck :: T.Var -> Type -> Bool
occursCheck v t = v `EnumSet.member` Subst.ftv t
