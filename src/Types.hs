{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE KindSignatures #-}


module Types
  ( Config(..)
  , ConfigElement (..)
  , Env (..)
  , App (..)
  ) where


import Colog (HasLog (..), LogAction, Message)
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT (..))
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Set (Set)

data Env m = Env
    { envLogAction  :: LogAction m Message
    -- , actionMode :: forall (t :: * -> *) (m :: * -> *) a b . t a -> (a -> m b) -> m (t b)
    }

instance HasLog (Env m) Message m where
    getLogAction :: Env m -> LogAction m Message
    getLogAction = envLogAction
    {-# INLINE getLogAction #-}

    setLogAction :: LogAction m Message -> Env m -> Env m
    setLogAction newLogAction env = env { envLogAction = newLogAction }
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

data Config = Config
  { taregetDirectory :: FilePath
  , configElement    :: [ConfigElement]
  }
  deriving stock (Show, Eq)

data ConfigElement = ConfigElement
  { hasFiles       :: Set FilePath
  , hasDirectories :: Set FilePath
  , hasExtensions  :: Set String
  , command        :: String
  , args           :: [String]
  }
  deriving stock (Show, Eq, Ord)
