module DNAnts.Defer where

import Control.Exception.Safe
       (Exception, Handler(Handler), MonadCatch,
        SomeException(SomeException), catch, catches, throw)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)

type DeferM m = WriterT (IO ()) m

type Defer m a = DeferM m a

-- | Register an action that should be run later.
--
defer :: (Monad m) => IO () -> Defer m ()
defer = tell

-- | Register an action that should be run later.
-- Use @deferE@ instead of @defer@ inside @ExceptT@.
deferE :: (Monad m) => IO () -> ExceptT e (DeferM m) ()
deferE = lift . defer

-- | Ensures to run deferred actions even after an error has been thrown.
--
runDefer :: (MonadIO m, MonadCatch m) => Defer m a -> m a
runDefer fn = do
  (result, deferredActions) <- runWriterT fn
  liftIO $ do
    putStrLn "run deferred actions"
    deferredActions
  return result

-- | Catch all errors that might be thrown in @f@.
--
catchIOError :: (MonadIO m) => IO a -> ExceptT SomeException m a
catchIOError f = do
  r <- liftIO (catch (Right <$> f) (return . Left))
  case r of
    (Left e) -> throwE e
    (Right c) -> return c