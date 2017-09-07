{-# LANGUAGE TupleSections #-}

module DNAnts.Lens where

import Control.Lens
import Control.Lens.Internal.Zoom (Focusing)
import Control.Lens.Operators
import Control.Lens.Traversal
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
       (StateT(StateT), execStateT, get, put)

{- |
Call monadic function with current state of @StateT@.

>>> flip execStateT "state" $ getsM putStrLn
state
"state"

-}
getsM :: Monad m => (s -> m a) -> StateT s m a
getsM f = StateT $ \s -> (, s) <$> f s

{- |
Lens of state in @StateT@.

>>> 42 ^. this
42

-}
this :: Lens' a a
this = lens id $ flip const

{- |
Run monadic action on state of a getter lens.

>>> flip execStateT ("first",2) $ viewM _1 putStrLn
first
("first",2)

-}
viewM :: Monad m => Getting t s t -> (t -> m a) -> StateT s m a
viewM g f = StateT $ \s -> (, s) <$> f (view g s)

{- |
Run monadic action on state of a focusing lens like.

>>> flip execStateT [(0,1),(2,3)] $ viewEachM (traverse . _1) $ putStrLn . show
0
2
[(0,1),(2,3)]

-}
viewEachM ::
     Monad m => LensLike' (Focusing m c) t s -> (s -> m c) -> StateT t m c
viewEachM g f = zoom g $ this .=> f

infix 4 .=>, .=>>

{- |
Operator alias of @viewM@ to run monadic action on state of a getter lens.

>>> flip execStateT ("first",2) $ _1 .=> putStrLn
first
("first",2)

-}
(.=>) :: Monad m => Getting t s t -> (t -> m a) -> StateT s m a
g .=> f = viewM g f

{- |
Operator alias of @viewEachM@ to run monadic action on state of a focusing lens
like.

>>> flip execStateT [(0,1),(2,3)] $ traverse . _1 .=>> putStrLn . show
0
2
[(0,1),(2,3)]

-}
(.=>>) :: Monad m => LensLike' (Focusing m c) t s -> (s -> m c) -> StateT t m c
g .=>> f = viewEachM g f