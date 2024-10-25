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

import Colog (HasLog (..), LogAction, Message)
import Control.Monad.Base (MonadBase)
import Control.Exception.Safe (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT (..))
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Set (Set)
import GHC.IO.Exception (ExitCode (..))
import Path.Posix (Abs, Dir, Path)

type ActionMode = [Path Abs Dir] -> (Path Abs Dir -> App ExitCode) -> App [ExitCode]

data Env m = Env
  { envLogAction :: LogAction m Message
  , actionMode :: ActionMode
  }

instance HasLog (Env m) Message m where
  getLogAction :: Env m -> LogAction m Message
  getLogAction = envLogAction
  {-# INLINE getLogAction #-}

  setLogAction :: LogAction m Message -> Env m -> Env m
  setLogAction newLogAction env = env{envLogAction = newLogAction}
  {-# INLINE setLogAction #-}

newtype App a = App
  { unApp :: ReaderT (Env App) IO a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader (Env App)
    , MonadMask
    , MonadCatch
    , MonadThrow
    , MonadBaseControl IO
    , MonadBase IO
    )


