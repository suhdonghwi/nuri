module Nuri.Eval.Flow where

import           Control.Monad
import           Control.Monad.Trans

data Flow r a = Thrown r
              | Normal a
  deriving (Eq, Show)

instance Functor (Flow r) where
  fmap = liftM

instance Applicative (Flow r) where
  pure  = return
  (<*>) = ap

instance Monad (Flow r) where
  return = Normal
  (Thrown v) >>= _ = Thrown v
  (Normal v) >>= f = f v

newtype FlowT r m a = FlowT { runFlowT :: m (Flow r a) }

instance Monad m => Functor (FlowT r m) where
  fmap = liftM

instance Monad m => Applicative (FlowT r m) where
  pure  = return
  (<*>) = ap

instance Monad m => Monad (FlowT r m) where
  return = FlowT . return . Normal
  x >>= f = FlowT $ do
    val <- runFlowT x
    case val of
      Thrown v -> return $ Thrown v
      Normal v -> runFlowT $ f v

instance MonadTrans (FlowT r) where
  lift = FlowT . (liftM Normal)

throw :: (Monad m) => r -> FlowT r m a
throw = FlowT . return . Thrown
