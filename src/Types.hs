{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Types (
    App (..),
    Env (..),
    Conditions (..),
    Config (..),
    ActionMode,
    forConcurrentlyKi,
) where

import Control.Exception.Safe (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Base (MonadBase)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (
    MonadReader,
    ReaderT (..),
    asks,
    local,
 )
import Control.Monad.Trans.Control (MonadBaseControl, StM, control)
import Control.Monad.Base (MonadBase (liftBase))
import Data.Set (Set)
import Dhall (FromDhall (..))
import GHC.Generics (Generic)
import GHC.IO.Exception (ExitCode (..))
import Katip (Katip (..), KatipContext (..), LogContexts, LogEnv (..), Namespace, Severity, Verbosity)
import Path.Posix (Abs, Dir, Path)
import Ki
import Control.Concurrent.STM (atomically)


newtype App a = MkApp
    { unApp :: ReaderT Env IO a
    }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader Env
        , MonadMask
        , MonadCatch
        , MonadThrow
        , MonadFail
        , MonadBaseControl IO
        , MonadBase IO
        )

instance Katip App where
    getLogEnv :: App LogEnv
    getLogEnv = asks envLogEnv
    localLogEnv :: (LogEnv -> LogEnv) -> App a -> App a
    localLogEnv f (MkApp m) =
        MkApp (local (\s -> s{envLogEnv = f (envLogEnv s)}) m)

instance KatipContext App where
    getKatipContext :: App LogContexts
    getKatipContext = asks envLogContext
    localKatipContext :: (LogContexts -> LogContexts) -> App a -> App a
    localKatipContext f (MkApp m) =
        MkApp (local (\s -> s{envLogContext = f (envLogContext s)}) m)
    getKatipNamespace :: App Namespace
    getKatipNamespace = asks envLogNamespace
    localKatipNamespace :: (Namespace -> Namespace) -> App a -> App a
    localKatipNamespace f (MkApp m) =
        MkApp (local (\s -> s{envLogNamespace = f (envLogNamespace s)}) m)

type ActionMode = [Path Abs Dir] -> (Path Abs Dir -> App ExitCode) -> App [ExitCode]

data Env = Env
    { envLogEnv :: LogEnv
    , envActionMode :: ActionMode
    , envLogContext :: LogContexts
    , envLogNamespace :: Namespace
    , envTarget :: FilePath
    , envConditions :: [Conditions]
    }

data Config = Config
    { cTarget :: FilePath
    , cConditions :: [Conditions]
    , cAsync :: Bool
    , cSeverity :: Severity
    , cVerbosity :: Verbosity
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromDhall)

deriving anyclass instance FromDhall Verbosity
deriving anyclass instance FromDhall Severity

data Conditions = Conditions
    { hasFiles :: Set FilePath
    , hasDirectories :: Set FilePath
    , hasExtensions :: Set String
    , command :: String
    , args :: [String]
    }
    deriving stock (Eq, Show, Generic, Ord)
    deriving anyclass (FromDhall)

forConcurrentlyKi ::
  (MonadBaseControl IO m, StM m (Ki.Thread b) ~ Ki.Thread b, StM m b ~ b, MonadIO m) =>
  [a] ->
  (a -> m b) ->
  m [b]
forConcurrentlyKi ns f = control $ \unlift -> scopedM \scope -> unlift $ do
  threads <- mapM (forkM scope . f) ns
  mapM (liftBase . atomically . Ki.await) threads

forkM ::
  (MonadBaseControl IO m, StM m (Ki.Thread a) ~ Ki.Thread a, StM m a ~ a) =>
  Ki.Scope ->
  m a ->
  m (Ki.Thread a)
forkM scope action =
  control \unlift -> Ki.fork scope (unlift action)

scopedM ::
  (MonadBaseControl IO m,  StM m a ~ a) =>
  (Ki.Scope -> m a) ->
  m a
scopedM action =
  control \unlift -> Ki.scoped \scope -> unlift (action scope)