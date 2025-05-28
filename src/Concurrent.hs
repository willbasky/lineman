{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Concurrent (
    forConcurrentlyKi,
    forConcurrentlyKi_,
) where

import Control.Concurrent.STM (atomically)
import Control.Monad.Base (MonadBase (liftBase))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl, StM, control)
import Ki

forConcurrentlyKi
    :: (MonadBaseControl IO m, StM m (Ki.Thread b) ~ Ki.Thread b, StM m b ~ b, MonadIO m)
    => [a]
    -> (a -> m b)
    -> m [b]
forConcurrentlyKi ns f = control $ \unlift -> scopedM \scope -> unlift $ do
    threads <- mapM (forkM scope . f) ns
    mapM (liftBase . atomically . Ki.await) threads

forConcurrentlyKi_
    :: (MonadBaseControl IO m, StM m (Ki.Thread b) ~ Ki.Thread b, StM m b ~ b, MonadIO m)
    => [a]
    -> (a -> m b)
    -> m ()
forConcurrentlyKi_ ns f = control $ \unlift -> scopedM \scope -> unlift $ do
    threads <- mapM (forkM scope . f) ns
    mapM_ (liftBase . atomically . Ki.await) threads

forkM
    :: (MonadBaseControl IO m, StM m (Ki.Thread a) ~ Ki.Thread a, StM m a ~ a)
    => Ki.Scope
    -> m a
    -> m (Ki.Thread a)
forkM scope action =
    control \unlift -> Ki.fork scope (unlift action)

scopedM
    :: (MonadBaseControl IO m, StM m a ~ a)
    => (Ki.Scope -> m a)
    -> m a
scopedM action =
    control \unlift -> Ki.scoped \scope -> unlift (action scope)
