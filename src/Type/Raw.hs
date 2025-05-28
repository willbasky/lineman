{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Type.Raw (
    RawCondition (..),
    Config (..),
)
where

import Data.Set (Set)
import Dhall (FromDhall (..))
import GHC.Generics (Generic)
import Katip (Severity, Verbosity)

data RawCondition = RawCondition
    { rcIndex :: Word
    , rcTarget :: FilePath
    , rcHasFiles :: Set FilePath
    , rcHasDirectories :: Set FilePath
    , rcHasExtensions :: Set String
    , rcCommand :: String
    , rcArgs :: [String]
    , rcActConcurrent :: Bool
    , rcWithBreak :: Double
    }
    deriving stock (Eq, Show, Generic, Ord)
    deriving anyclass (FromDhall)

data Config = Config
    { confRawConditions :: [RawCondition]
    , confSeverity :: Severity
    , confVerbosity :: Verbosity
    , confSwarmConcurrent :: Bool
    , confSwarmBreak :: Double
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromDhall)

deriving anyclass instance FromDhall Verbosity

deriving anyclass instance FromDhall Severity
