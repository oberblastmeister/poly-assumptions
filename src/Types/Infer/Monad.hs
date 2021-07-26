module Types.Infer.Monad
  ( MonadInfer,
    InferState (..),
    assumptions,
    constraints,
    boundVars,
    Infer,
    runInfer,
    TypeError (..),
    Constraint (..),
    addConstraint,
    addConstraints,
    removeAssumptions,
    lookupAssumptions,
    addAssumption,
    freshV,
    freshT,
    varSupply,
    defInferState,
    takeState,
    listenState,
    localState,
  )
where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Supply
import Data.DList (DList)
import qualified Data.DList as DL
import Data.EnumSet (EnumSet)
import qualified Data.EnumSet as ESet
import Data.Foldable (toList)
import Lens.Micro ((%~), (&))
import Lens.Micro.TH
import Prettyprinter (Pretty (pretty), (<+>))
import Protolude
import Types.Assumptions (Assumptions)
import qualified Types.Assumptions as Assumptions
import Types.Type (Type)
import qualified Types.Type as T
import Data.HashSet (HashSet)

type Infer =
  ( StateT
      InferState
      (SupplyT T.Var (Either TypeError))
  )

data InferState = InferState
  { _assumptions :: Assumptions,
    _constraints :: DList Constraint,
    _boundVars :: EnumSet T.Var
  }
  deriving (Show)

type MonadInfer m =
  ( MonadSupply T.Var m,
    MonadError TypeError m,
    MonadState InferState m
  )

data TypeError
  = TypeMismatch Type Type
  | TypeMismatchMulti [Type] [Type]
  | InfiniteType T.Var Type
  | UnboundVariable Text
  | UnsolvableConstraints [Constraint]
  deriving (Show, Eq)

data Constraint
  = ConEqual Type Type
  | ConExplicit Type T.Scheme
  | ConImplicit Type (HashSet Type) Type
  deriving (Show, Eq)

instance Pretty Constraint where
  pretty (ConEqual t1 t2) = pretty t1 <+> "~" <+> pretty t2
  pretty (ConExplicit t1 t2) = pretty t1 <+> "<=" <+> pretty t2
  pretty (ConImplicit t1 vs t2) = pretty t1 <+> "gen(" <> pretty t2 <> ")"

makeLenses 'InferState

varSupply :: [T.Var]
varSupply = T.VarId <$> [1 ..]

freshV :: MonadSupply T.Var m => m T.Var
freshV = supply

freshT :: MonadSupply T.Var m => m Type
freshT = T.Var <$> supply

defInferState :: InferState
defInferState =
  InferState
    { _assumptions =
        Assumptions.empty,
      _constraints = DL.empty,
      _boundVars = ESet.empty
    }

takeState :: (MonadState InferState m) => StateT InferState m a -> m (a, InferState)
takeState m = do
  curr <- get
  runStateT m curr

listenState :: (MonadState InferState m) => StateT InferState m a -> m (a, InferState)
listenState m = do
  (a, s) <- takeState m
  put s
  return (a, s)

runInfer :: (MonadError TypeError m) => Infer a -> m (a, InferState)
runInfer m =
  liftEither $
    evalSupplyT
      ( runStateT
          m
          -- (runReaderT m EnumSet.empty)
          defInferState
      )
      varSupply

addConstraints :: InferState -> [Constraint] -> InferState
addConstraints st cs =
  st & constraints %~ (`DL.append` DL.fromList cs)

addConstraint :: Constraint -> InferState -> InferState
addConstraint c =
  constraints %~ (`DL.snoc` c)

removeAssumptions :: Text -> InferState -> InferState
removeAssumptions n =
  assumptions %~ (`Assumptions.remove` n)

addAssumption :: (Text, Type) -> InferState -> InferState
addAssumption a = assumptions %~ (`Assumptions.add` a)

lookupAssumptions :: Text -> InferState -> [Type]
lookupAssumptions x = toList . Assumptions.lookup x . _assumptions

localState :: forall s m a. MonadState s m => (s -> s) -> m a -> m a
localState f action = do
  orig <- get
  put $ f orig
  a <- action
  put orig
  return a

-- withModifyState :: forall s m a. MonadState InferState m => (s -> s) -> m a -> m a
-- withModifyState f action = do
--   st <- get
--   withState (f st) action

-- | have the monomorphic variable inside of the monomorphic set locally for this computation
-- withBoundVar :: MonadState InferState m => T.Var -> m a -> m a
-- withBoundVar v = localState (

-- withMVar :: MonadReader (EnumSet T.Var) m => T.Var -> m a -> m a
-- withMVar v = local (EnumSet.insert v)
