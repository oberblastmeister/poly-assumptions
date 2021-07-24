{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Supply
  ( evalSupplyT,
    evalSupply,
    runSupplyT,
    runSupply,
    MonadSupply (..),
    SupplyT,
    Supply,
  )
where

import Control.Monad.Except (ExceptT (ExceptT), MonadError (..), MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Identity
import Control.Monad.Reader (ReaderT (ReaderT))
import Control.Monad.State (StateT (runStateT), evalStateT, get, gets, put)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import qualified Control.Monad.Trans.State.Lazy as LazyState
import Control.Monad.Writer (WriterT (WriterT))
import Prelude hiding (tail)

-- | The supply transformer.
newtype SupplyT s m a = SupplyT {unSupplyT :: StateT [s] m a}
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadFix)

-- | The supply monad which is SupplyT specialized with the Maybe monad for the MonadFix instance.
-- | This is because the supply might be finite so it can fail.
-- | means in order to run this you will also have to unwrap the Maybe.
newtype Supply s a = Supply (SupplyT s Identity a)
  deriving (Functor, Applicative, Monad, MonadSupply s, MonadFix)

evalSupplyT :: Monad m => SupplyT s m a -> [s] -> m a
evalSupplyT (SupplyT s) = evalStateT s

evalSupply :: Supply s a -> [s] -> a
evalSupply (Supply s) = runIdentity . evalSupplyT s

runSupplyT :: SupplyT s m a -> [s] -> m (a, [s])
runSupplyT (SupplyT s) = runStateT s

runSupply :: Supply s a -> [s] -> (a, [s])
runSupply (Supply s) = runIdentity . runSupplyT s

instance MonadError e m => MonadError e (SupplyT s m) where
  throwError = lift . throwError

  catchError m h =
    SupplyT $
      LazyState.liftCatch catchError (unSupplyT m) (unSupplyT . h)

class Monad m => MonadSupply s m | m -> s where
  supply :: m s
  isExhausted :: m Bool

instance Monad m => MonadSupply s (SupplyT s m) where
  supply = SupplyT $ do
    (x, xs) <- gets split
    put xs
    return x
    where
      split (x : xs) = (x, xs)
      split _ = error "no more supply"

  isExhausted = SupplyT $ gets null

instance MonadSupply s m => MonadSupply s (ExceptT e m) where
  supply = lift supply
  isExhausted = lift isExhausted

instance MonadSupply s m => MonadSupply s (StateT st m) where
  supply = lift supply
  isExhausted = lift isExhausted

instance MonadSupply s m => MonadSupply s (ReaderT r m) where
  supply = lift supply
  isExhausted = lift isExhausted

instance MonadSupply s m => MonadSupply s (MaybeT m) where
  supply = lift supply
  isExhausted = lift isExhausted

instance (Monoid w, MonadSupply s m) => MonadSupply s (WriterT w m) where
  supply = lift supply
  isExhausted = lift isExhausted
