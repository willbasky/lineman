{-# LANGUAGE DerivingStrategies #-}
module Lineman where

import Relude
-- import Relude.String

import Path.IO
import Path.Posix


dirForClean :: IO [Path Abs File]
dirForClean = do
  dir <- parseAbsDir "/home/metaxis/sources/Haskell/"
  stack <- parseRelFile "stack.yaml"
  print stack
  print dir
  (dirs, _) <- listDirRecur dir
  findFiles dirs stack



data Config = Config
  { directory :: FilePath
  , command :: [Text]
  , isFiles :: [FilePath]
  , deepLevel :: Word8
  }
  deriving stock (Show, Eq)
