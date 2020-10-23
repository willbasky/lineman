{-# LANGUAGE DerivingStrategies #-}
module Lineman
    (
    ) where

import Relude
import Relude.String

import Path.IO hiding (findFiles, findFilesWith)
import Path.Posix


dirForClean :: IO [Path Abs File]
dirForClean = do
  dir <- parseAbsDir "/home/metaxis/sources/Haskell/"
  stack <- parseRelFile "stack.yaml"
  print stack
  print dir
  (dirs, files) <- listDirRecur dir
  findFiles dirs stack



data Config = Config
  { directory :: FilePath
  , command :: [Text]
  , isFiles :: [FilePath]
  , deepLevel :: Word8
  }
  deriving stock (Show, Eq)












-- Used here until origin repo be not fixed

findFiles ::
  MonadIO m =>
  -- | Set of directories to search in
  [Path b Dir] ->
  -- | Filename of interest
  Path Rel File ->
  -- | Absolute paths to all found files
  m [Path Abs File]
findFiles = findFilesWith (const (return True))

-- | Search through the given set of directories for the given file and with
-- the given property (usually permissions) and return a list of paths where
-- the given file exists and has the property.
findFilesWith ::
  MonadIO m =>
  -- | How to test the files
  (Path Abs File -> m Bool) ->
  -- | Set of directories to search in
  [Path b Dir] ->
  -- | Filename of interest
  Path Rel File ->
  -- | Absolute paths to all found files
  m [Path Abs File]
findFilesWith _ [] _ = return []
findFilesWith f (d : ds) file = do
  bfile <- (</> file) <$> makeAbsolute d
  exist <- doesFileExist bfile
  b <- if exist then f bfile else return False
  if b
    then (bfile :) <$> findFilesWith f ds file
    else findFilesWith f ds file














