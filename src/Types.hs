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
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Types (
    App (..),
    Env (..),
    Conditions (..),
    Config (..),
    ActionMode,
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
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Set (Set)
import Dhall (FromDhall (..))
import GHC.Generics (Generic)
import GHC.IO.Exception (ExitCode (..))
import Katip (Katip (..), KatipContext (..), LogContexts, LogEnv (..), Namespace, Severity, Verbosity)
import Path.Posix (Abs, Dir, Path)

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
    getLogEnv = asks envLogEnv
    localLogEnv f (MkApp m) =
        MkApp (local (\s -> s{envLogEnv = f (envLogEnv s)}) m)

instance KatipContext App where
    getKatipContext = asks envLogContext
    localKatipContext f (MkApp m) =
        MkApp (local (\s -> s{envLogContext = f (envLogContext s)}) m)
    getKatipNamespace = asks envLogNamespace
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
