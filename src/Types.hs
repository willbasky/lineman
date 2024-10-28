{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Types (
    Env (..),
    App (..),
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
import GHC.IO.Exception (ExitCode (..))
import Katip
import Path.Posix (Abs, Dir, Path)

type ActionMode = [Path Abs Dir] -> (Path Abs Dir -> App ExitCode) -> App [ExitCode]

data Env = Env
    { envLogEnv :: LogEnv
    , envActionMode :: ActionMode
    , envLogContext :: LogContexts
    , envLogNamespace :: Namespace
    }

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
    MkApp (local (\s -> s {envLogContext = f (envLogContext s)}) m)
  getKatipNamespace = asks envLogNamespace
  localKatipNamespace f (MkApp m) =
    MkApp (local (\s -> s {envLogNamespace = f (envLogNamespace s)}) m)
