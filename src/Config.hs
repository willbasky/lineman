{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module Config (
    Config (..),
    Conditions (..),
    getConfig,
) where

import qualified Colog.Core as Colog
import Control.Exception.Safe (throwIO, tryAny)
import Data.Set (Set)
import Dhall (FromDhall (..), auto, inputFile)
import GHC.Generics (Generic)
import Text.Pretty.Simple (pPrintString)

-- dhall type
-- co-log Severity hasn't Generic and Rep instances.
-- Todo: consider using 'katip'
data ConfigD = ConfigD
    { cdTarget :: FilePath
    , cdConditions :: [Conditions]
    , cdAsync :: Bool
    , cdSeverity :: Severity
    , cdRichLog :: Bool
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromDhall)

-- Help type for decoding from dhall
-- do not export
data Severity = Debug | Info | Warning | Error
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromDhall)

data Conditions = Conditions
    { hasFiles :: Set FilePath
    , hasDirectories :: Set FilePath
    , hasExtensions :: Set String
    , command :: String
    , args :: [String]
    }
    deriving stock (Eq, Show, Generic, Ord)
    deriving anyclass (FromDhall)

-- domain type.
data Config = Config
    { cTarget :: FilePath
    , cConditions :: [Conditions]
    , cAsync :: Bool
    , cSeverity :: Colog.Severity
    , cRichLog :: Bool
    }
    deriving stock (Eq, Show, Generic)

fromConfigD :: ConfigD -> Config
fromConfigD ConfigD{..} = Config
    { cTarget = cdTarget
    , cConditions = cdConditions
    , cAsync = cdAsync
    , cSeverity = fromSeverityD cdSeverity
    , cRichLog = cdRichLog
    }
  where
    fromSeverityD :: Severity -> Colog.Severity
    fromSeverityD = \case
        Debug -> Colog.Debug
        Info -> Colog.Info
        Warning -> Colog.Warning
        Error -> Colog.Error

getConfig :: FilePath -> IO Config
getConfig path = do
    eConfigD :: Either a ConfigD <- tryAny $ inputFile auto path
    case eConfigD of
        Left err -> do
            pPrintString "Config parsing failed"
            throwIO err
        Right decoded -> pure $ fromConfigD decoded
