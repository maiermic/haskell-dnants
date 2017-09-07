{-# LANGUAGE TupleSections #-}

module DNAnts.Lens where

import Control.Lens
import Control.Lens.Internal.Zoom (Focusing)
import Control.Lens.Operators
import Control.Lens.Traversal
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
       (StateT(StateT), execStateT, get, put)

getsM :: Monad m => (s -> m a) -> StateT s m a
getsM f = StateT $ \s -> (, s) <$> f s

this :: Lens' a a
this = lens id $ flip const

viewM :: Monad m => Getting t s t -> (t -> m a) -> StateT s m a
viewM g f = StateT $ \s -> (, s) <$> f (view g s)

viewEachM ::
     Monad m => LensLike' (Focusing m c) t s -> (s -> m c) -> StateT t m c
viewEachM g f = zoom g $ this .=> f

infix 4 .=>, .=>>

(.=>) :: Monad m => Getting t s t -> (t -> m a) -> StateT s m a
g .=> f = viewM g f

(.=>>) :: Monad m => LensLike' (Focusing m c) t s -> (s -> m c) -> StateT t m c
g .=>> f = viewEachM g f