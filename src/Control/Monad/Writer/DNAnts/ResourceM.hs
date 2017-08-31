module Control.Monad.Writer.DNAnts.ResourceM where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)

type ResourceM m a = WriterT (IO ()) m a

-- | Run a WriterT that releases all resources at the end.
-- Just tell it how to release your resource:
--
-- @
-- runResourceM $ do
--   r <- requestResource
--   onReleaseResources $ releaseResource r
--   liftIO $ doSomethingWith r
-- @
--
runResourceM :: MonadIO m => ResourceM m a -> m a
runResourceM f = do
  (result, release) <- runWriterT f
  liftIO release
  return result

-- | @'onReleaseResources' r@ is an action that releases a resource.
-- It is just an alias of @tell@.
onReleaseResources :: (Monad m) => IO () -> ResourceM m ()
onReleaseResources = tell

{-# INLINE onReleaseResources #-}