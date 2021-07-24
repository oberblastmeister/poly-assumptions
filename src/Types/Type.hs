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

import Data.EnumSet (EnumSet)
import Data.Text (Text)
import Pretty (parensIf)
import Prettyprinter (Pretty, pretty, (<+>))

newtype Var = VarId {unVarId :: Int}
  deriving (Show, Eq, Enum, Pretty)

data Type
  = Con Text
  | Var Var
  | Type :-> Type
  deriving (Show, Eq)

isArr :: Type -> Bool
isArr (_ :-> _) = True
isArr _ = False

instance Pretty Type where
  pretty x = case x of
    Con name -> pretty name
    Var name -> pretty name
    t1 :-> t2 -> parensIfArr t1 <+> "->" <+> pretty t2
    where
      parensIfArr t = parensIf (isArr t) (pretty t)

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
  = Forall [Var] Type
  deriving (Show, Eq)

instance Pretty Scheme where
  pretty (Forall as t) = "forall" <+> pretty as <> "." <+> pretty t

-- smart constructors
var :: Int -> Type
var = Var . VarId

int, bool :: Type
int = Con "Int"
bool = Con "Bool"
