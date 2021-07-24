module Types.Infer.Monad
  ( MonadInfer,
    InferState (..),
    assumptions,
    constraints,
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
    withMVar,
    varSupply,
    defInferState,
    takeState,
    listenState,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Supply
import Data.DList (DList)
import qualified Data.DList as DL
import Data.EnumSet (EnumSet)
import qualified Data.EnumSet as EnumSet
import Data.Sequence (Seq)
import Lens.Micro ((%~), (&))
import Lens.Micro.TH
import Protolude
import Types.Assumptions (Assumptions)
import qualified Types.Assumptions as Assumptions
import Types.Type (Type)
import qualified Types.Type as T

type Infer =
  ( ReaderT
      (EnumSet T.Var)
      ( StateT
          InferState
          (SupplyT T.Var (Either TypeError))
      )
  )

data InferState = InferState
  { _assumptions :: Assumptions,
    _constraints :: DList Constraint
  }

type MonadInfer m =
  ( MonadReader (EnumSet T.Var) m,
    MonadSupply T.Var m,
    MonadError TypeError m,
    MonadState InferState m
  )

data TypeError
  = TypeMismatch Type Type
  | TypeMismatchMulti [Type] [Type]
  | InfiniteType T.Var Type
  | UnboundVariable Text
  deriving (Show, Eq)

data Constraint
  = ConEqual Type Type
  | ConExplicit Type T.Scheme
  | ConImplicit Type (EnumSet T.Var) Type
  deriving (Show, Eq)

makeLenses 'InferState

varSupply :: [T.Var]
varSupply = T.VarId <$> [1 ..]

freshV :: MonadSupply T.Var m => m T.Var
freshV = supply

freshT :: MonadSupply T.Var m => m Type
freshT = T.Var <$> supply

defInferState :: InferState
defInferState = InferState {_assumptions = Assumptions.empty, _constraints = DL.empty}

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
          (runReaderT m EnumSet.empty)
          defInferState
      )
      varSupply

addConstraints :: InferState -> [Constraint] -> InferState
addConstraints st cs =
  st & constraints %~ (`DL.append` DL.fromList cs)

addConstraint :: MonadState InferState m => Constraint -> m ()
addConstraint c =
  modify (constraints %~ (`DL.snoc` c))

removeAssumptions :: InferState -> Text -> InferState
removeAssumptions st n =
  st & assumptions %~ (`Assumptions.remove` n)

addAssumption :: MonadState InferState m => (Text, Type) -> m ()
addAssumption a =
  modify (assumptions %~ (`Assumptions.add` a))

lookupAssumptions :: MonadState InferState m => Text -> m (Seq Type)
lookupAssumptions x = gets (Assumptions.lookup x . _assumptions)

-- | have the monomorphic variable inside of the monomorphic set locally for this computation
withMVar :: MonadReader (EnumSet T.Var) m => T.Var -> m a -> m a
withMVar v = local (EnumSet.insert v)
