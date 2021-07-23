module Types.Type
  ( Type (..),
    Scheme (..),
    int,
    bool,
    var,
    Var (..),
    everywhere,
    everything,
  )
where

import Prelude hiding (Type)
import Data.EnumSet (EnumSet)
import Data.Text (Text)

newtype Var = VarId {unVarId :: Int}
  deriving (Show, Eq, Enum)

data Type
  = Con Text
  | Var Var
  | Type :-> Type
  deriving (Show, Eq)

infixr 9 :->

everywhere :: (Type -> Type) -> Type -> Type
everywhere f = go
  where
    go (j1 :-> j2) = f $ go j1 :-> go j2
    go other = f other

everything :: (r -> r -> r) -> (Type -> r) -> Type -> r
everything (<>.) f = go
  where
    go j@(j1 :-> j2) = f j <>. go j1 <>. go j2
    go other = f other

data Scheme
  = Forall (EnumSet Var) Type
  deriving (Show, Eq)

-- smart constructors
var :: Int -> Type
var = Var . VarId

int, bool :: Type
int = Con "Int"
bool = Con "Bool"
