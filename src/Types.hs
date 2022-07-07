{-# LANGUAGE DerivingStrategies #-}

module Types
  ( Config(..)
  , ConfigElement (..)
  ) where

import Data.Set ( Set )

data Config = Config
  { taregetDirectory :: FilePath
  , configElement    :: Set ConfigElement
  }
  deriving stock (Show, Eq)

data ConfigElement = ConfigElement
  { hasFiles         :: Set FilePath
  , hasDirectories   :: Set FilePath
  , hasExtensions    :: Set String
  , command          :: String
  , args             :: Set String
  }
  deriving stock (Show, Eq, Ord)
