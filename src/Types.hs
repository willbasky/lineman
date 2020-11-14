{-# LANGUAGE DerivingStrategies #-}

module Types
  ( Config(..)
  ) where


data Config = Config
  { taregetDirectory :: FilePath
  , hasFiles         :: [FilePath]
  , hasDirectories   :: [FilePath]
  , hasExtensions    :: [String]
  , command          :: String
  , args             :: [String]
  }
  deriving stock (Show, Eq)

