{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Type.Domain (
    App (..),
    Env (..),
    Condition (..),
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
import Katip (Katip (..), KatipContext (..), LogContexts, LogEnv (..), Namespace)
import Path (File, Rel)
import Path.Posix (Abs, Dir, Path)
import Data.List.NonEmpty (NonEmpty)

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

-- type ActionMode = [Path Abs Dir] -> (Path Abs Dir -> App ExitCode) -> App [ExitCode]

data Env = Env
    { envLogEnv :: LogEnv
    , envLogContext :: LogContexts
    , envLogNamespace :: Namespace
    , envConditions :: NonEmpty Condition
    , envSwarmConcurrent :: Bool
    , envSwarmBreak :: Double
    }

data Condition = Condition
    { cIndex :: Word
    , cTarget :: Maybe (Path Abs Dir)
    , cFiles :: Maybe [Path Rel File]
    , cDirectories :: [Path Rel Dir]
    , cExtensions :: [String]
    , cCommand :: String
    , cArguments :: [String]
    , cActConcurrent :: Bool
    , cWithBreak :: Double
    }
    deriving stock (Show, Eq)
