{-# LANGUAGE TupleSections #-}

module DNAnts.Lens where

import Control.Lens
import Control.Lens.Operators
import Control.Lens.Traversal
import Control.Monad.Trans.State.Lazy
       (StateT(StateT), execStateT, get, put)

getsM :: Monad m => (s -> m a) -> StateT s m a
getsM f = StateT $ \s -> (, s) <$> f s

viewM :: Functor m => Getting t s t -> (t -> m a) -> StateT s m a
viewM g f = StateT $ \s -> (, s) <$> f (view g s)

infix 4 .=>

(.=>) :: Functor m => Getting t s t -> (t -> m a) -> StateT s m a
g .=> f = viewM g f