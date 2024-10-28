{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Config (
    Config (..),
    Conditions (..),
    getConfig,
) where

import Control.Exception.Safe (throwIO, tryAny)
import Data.Set (Set)
import Dhall (FromDhall (..), auto, inputFile)
import GHC.Generics (Generic)
import Katip (Severity, Verbosity)
import Text.Pretty.Simple (pPrintString)

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

getConfig :: FilePath -> IO Config
getConfig path = do
    eConfig :: Either a Config <- tryAny $ inputFile auto path
    case eConfig of
        Left err -> do
            pPrintString "Config parsing failed"
            throwIO err
        Right decoded -> pure decoded
