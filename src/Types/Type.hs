module Types.Type
  ( Type (..),
    Scheme (..),
    int,
    bool,
    var,
    Var (..),
    prettyRaw,
    everywhere,
    everything,
  )
where

import Control.Monad.Identity (runIdentity)
import Control.Monad.State
import Control.Monad.Supply
import Data.EnumMap (EnumMap)
import qualified Data.EnumMap as EMap
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Pretty (parensIf)
import Prettyprinter (Doc, Pretty, pretty, (<+>))
import qualified Prettyprinter as P
import qualified Prettyprinter.Render.Text as P.Render.Text
import Prelude hiding (lookup, map)

newtype Var = VarId {unVarId :: Int}
  deriving (Show, Eq, Enum, Pretty, Hashable)

ppRawVar :: Var -> Doc ann
ppRawVar (VarId i) = pretty i

data Type
  = Con Text
  | Var Var
  | Type :-> Type
  deriving (Eq, Generic)

instance Hashable Type

instance Show Type where
  show t =
    prettyRaw t
      & P.layoutPretty P.defaultLayoutOptions
      & P.Render.Text.renderStrict
      & T.unpack

isArr :: Type -> Bool
isArr (_ :-> _) = True
isArr _ = False

varNameSupply :: [Text]
varNameSupply = T.pack <$> ([1 ..] >>= flip replicateM ['a' .. 'z'])

prettyWith ::
  forall m ann.
  Monad m =>
  (Var -> m (Doc ann)) ->
  Type ->
  m (P.Doc ann)
prettyWith ppVar = \case
  Con name -> return $ pretty name
  Var v -> ppVar v
  t1 :-> t2 -> do
    t1' <- parensIfArr t1
    t2' <- prettyWith ppVar t2
    return $ t1' <+> "->" <+> t2'
  where
    parensIfArr t = prettyWith ppVar t <&> parensIf (isArr t)

instance Pretty Type where
  pretty x =
    prettyWith ppLookupVar x
      & (`evalStateT` EMap.empty)
      & (`evalSupply` varNameSupply)

prettyRaw :: Type -> P.Doc ann
prettyRaw t = prettyWith (return . ppRawVar) t & runIdentity

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
        prettyWith ppLookupVar t
          & (`runStateT` mapStart)
          & (`evalSupply` varNameSupply)

      mapStart = EMap.fromList $ zip as varNameSupply

ppLookupVar ::
  forall m ann.
  (MonadState (EnumMap Var Text) m, MonadSupply Text m) =>
  Var ->
  m (Doc ann)
ppLookupVar v = do
  map <- get
  case EMap.lookup v map of
    Just name -> return $ pretty name
    Nothing -> do
      name <- supply
      put $ EMap.insert v name map
      return $ pretty name

-- smart constructors
var :: Int -> Type
var = Var . VarId

int, bool :: Type
int = Con "Int"
bool = Con "Bool"
