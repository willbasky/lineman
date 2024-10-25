{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Config (
    Config (..),
    Conditions (..),
    getConfig,
) where

import Data.Set (Set)
import Dhall (FromDhall (..), auto, inputFile)
import GHC.Generics (Generic)
import Text.Pretty.Simple (pPrintString)
import Control.Exception.Safe (tryAny, throwIO)

-- import Toml (TomlCodec, (.=))
-- import qualified Toml

data Config = Config
    { targetDirectory :: FilePath
    , conditions :: [Conditions]
    }
    deriving stock (Eq, Show, Generic, Ord)
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

-- deriving anyclass instance FromDhall HyperTextProtocol
-- deriving anyclass instance FromDhall NetworkMagic
-- deriving anyclass instance FromDhall NetworkId
-- deriving anyclass instance FromDhall CurrencySymbol
-- deriving anyclass instance FromDhall Environment
-- deriving anyclass instance FromDhall Verbosity
-- deriving anyclass instance FromDhall Severity

getConfig :: FilePath -> IO Config
getConfig path = do
    eConfig <- tryAny $ inputFile auto path
    case eConfig of
        Left err -> do
            pPrintString "Toml parsing failed"
            throwIO err
        Right decoded -> pure decoded
