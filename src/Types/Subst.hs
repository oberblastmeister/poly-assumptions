module Types.Subst where

import Data.EnumMap (EnumMap)
import qualified Data.EnumMap as EnumMap
import Data.EnumSet (EnumSet)
import qualified Data.EnumSet as EnumSet
import Data.Maybe
import Types.Type (Type)
import qualified Types.Type as T
import Prelude hiding (lookup)

newtype Subst = Subst (EnumMap T.Var Type)
  deriving (Eq)

class Substitutable a where
  (@@) :: Subst -> a -> a
  ftv :: a -> EnumSet T.Var

instance Substitutable Type where
  su @@ t =
    T.everywhere
      ( \case
          tv@(T.Var v) -> fromMaybe tv (lookup su v)
          other -> other
      )
      t
  ftv =
    T.everything
      (<>)
      ( \case
          T.Var v -> EnumSet.singleton v
          _ -> mempty
      )

instance Substitutable T.Scheme where
  (Subst s) @@ (T.Forall as t) = T.Forall as $ s' @@ t
    where
      s' = Subst $ foldr EnumMap.delete s (EnumSet.toList as)
  ftv (T.Forall as t) = ftv t `EnumSet.difference` as

instance Semigroup Subst where
  (<>) = compose

instance Monoid Subst where
  mappend = (<>)
  mempty = empty

instance (Substitutable a, Enum a) => Substitutable (EnumSet a) where
  (@@) = EnumSet.map . (@@)
  ftv = EnumSet.foldr (EnumSet.union . ftv) EnumSet.empty

instance (Substitutable a) => Substitutable [a] where
  (@@) = map . (@@)
  ftv = foldr (EnumSet.union . ftv) EnumSet.empty

class ActiveTypesVars a where
  atv :: a -> EnumSet T.Var

compose :: Subst -> Subst -> Subst
(Subst s1) `compose` (Subst s2) = Subst $ EnumMap.map (Subst s1 @@) s2 `EnumMap.union` s1

empty :: Subst
empty = Subst EnumMap.empty

singleton :: T.Var -> Type -> Subst
singleton v = Subst . EnumMap.singleton v

lookup :: Subst -> T.Var -> Maybe Type
lookup (Subst m) v = EnumMap.lookup v m
