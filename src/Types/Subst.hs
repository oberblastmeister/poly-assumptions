module Types.Subst where

import Data.EnumMap (EnumMap)
import qualified Data.EnumMap as EMap
import Data.EnumSet (EnumSet)
import qualified Data.EnumSet as ESet
import Data.Maybe
import Types.Infer.Monad (Constraint (..))
import Types.Type (Type)
import qualified Types.Type as T
import Prelude hiding (lookup)

newtype Subst = Subst (EnumMap T.Var Type)
  deriving (Show, Eq)

class Substitutable a where
  (@@) :: Subst -> a -> a

instance Substitutable Subst where
  (Subst s1) @@ (Subst s2) = Subst $ EMap.map (Subst s1 @@) s2

instance Substitutable Type where
  su @@ t =
    T.everywhere
      ( \case
          tv@(T.Var v) -> fromMaybe tv (lookup su v)
          other -> other
      )
      t

instance Substitutable T.Scheme where
  (Subst s) @@ (T.Forall as t) = T.Forall as $ s' @@ t
    where
      s' = Subst $ foldr EMap.delete s as

instance Semigroup Subst where
  (<>) = compose

instance Monoid Subst where
  mappend = (<>)
  mempty = empty

instance Substitutable Constraint where
  s @@ (ConEqual t1 t2) = ConEqual (s @@ t1) (s @@ t2)
  s @@ (ConExplicit t sc) = ConExplicit (s @@ t) (s @@ sc)
  s @@ (ConImplicit t1 ms t2) = ConImplicit (s @@ t1) ms (s @@ t2)

instance (Substitutable a, Enum a) => Substitutable (EnumSet a) where
  (@@) = ESet.map . (@@)

instance (Substitutable a) => Substitutable [a] where
  (@@) = map . (@@)

class FreeTypeVars a where
  ftv :: a -> EnumSet T.Var

instance FreeTypeVars Type where
  ftv =
    T.everything
      (<>)
      ( \case
          T.Var v -> ESet.singleton v
          _ -> mempty
      )

instance FreeTypeVars T.Scheme where
  ftv (T.Forall as t) = ftv t `ESet.difference` ESet.fromList as

class ActiveTypeVars a where
  atv :: a -> EnumSet T.Var

instance (Enum a, FreeTypeVars a) => FreeTypeVars (EnumSet a) where
  ftv = ESet.foldr (ESet.union . ftv) ESet.empty

instance (FreeTypeVars a) => FreeTypeVars [a] where
  ftv = foldr (ESet.union . ftv) ESet.empty

instance ActiveTypeVars Constraint where
  atv (ConEqual t1 t2) = ftv t1 `ESet.union` ftv t2
  atv (ConImplicit t1 ms t2) = ftv t1 `ESet.union` (ms `ESet.intersection` ftv t2)
  atv (ConExplicit t s) = ftv t `ESet.union` ftv s

instance ActiveTypeVars a => ActiveTypeVars [a] where
  atv = foldr (ESet.union . atv) ESet.empty

compose :: Subst -> Subst -> Subst
su1@(Subst s1) `compose` su2@(Subst _) =
  let (Subst s3) = su1 @@ su2 in Subst (s3 `EMap.union` s1)

fromList :: [(T.Var, Type)] -> Subst
fromList = Subst . EMap.fromList

empty :: Subst
empty = Subst EMap.empty

singleton :: T.Var -> Type -> Subst
singleton v = Subst . EMap.singleton v

lookup :: Subst -> T.Var -> Maybe Type
lookup (Subst m) v = EMap.lookup v m
