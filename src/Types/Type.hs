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

import Control.Monad (replicateM)
import Control.Monad.State
import Control.Monad.Supply
import Data.EnumMap (EnumMap)
import qualified Data.EnumMap as EMap
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Debugging
import Pretty (parensIf)
import Prettyprinter (Pretty, pretty, (<+>))
import qualified Prettyprinter as P
import Prelude hiding (lookup, map)

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

varNameSupply :: [Text]
varNameSupply = T.pack <$> ([1 ..] >>= flip replicateM ['a' .. 'z'])

prettyWith ::
  forall m ann.
  (MonadState (EnumMap Var Text) m, MonadSupply Text m) =>
  Type ->
  m (P.Doc ann)
prettyWith = \case
  Con name -> return $ pretty name
  Var v -> pretty <$> lookupVar v
  t1 :-> t2 -> do
    t1' <- parensIfArr t1
    t2' <- prettyWith t2
    return $ t1' <+> "->" <+> t2'
  where
    parensIfArr t = prettyWith t <&> parensIf (isArr t)

    lookupVar :: Var -> m Text
    lookupVar v = do
      map <- get
      case EMap.lookup v map of
        Just name -> return name
        Nothing -> do
          name <- supply
          put $ EMap.insert v name map
          return name

instance Pretty Type where
  pretty x =
    prettyWith x
      & (`evalStateT` EMap.empty)
      & (`evalSupply` varNameSupply)

infixr 9 :->

everywhere :: (Type -> Type) -> Type -> Type
everywhere f = go
  where
    go = \case
      (j1 :-> j2) -> f $ go j1 :-> go j2
      other -> f other

everything :: (r -> r -> r) -> (Type -> r) -> Type -> r
everything (<>.) f = go
  where
    go = \case
      j@(j1 :-> j2) -> f j <>. go j1 <>. go j2
      other -> f other

data Scheme
  = Forall [Var] Type
  deriving (Show, Eq)

instance Pretty Scheme where
  pretty (Forall [] t) = pretty t
  pretty (Forall as t) = do
    "forall" <+> P.hsep (pretty . lookup <$> as) <> "." <+> doc
    where
      lookup v =
        fromMaybe (error $ "could not find var " ++ show v) $
          EMap.lookup v map

      (doc, map) =
        prettyWith t
          & (`runStateT` mapStart)
          & (`evalSupply` varNameSupply)

      mapStart = EMap.fromList $ zip as varNameSupply

-- smart constructors
var :: Int -> Type
var = Var . VarId

int, bool :: Type
int = Con "Int"
bool = Con "Bool"
