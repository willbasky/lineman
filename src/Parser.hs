{-# LANGUAGE TypeApplications #-}

module Parser (
    safeHead,
    prepareConditions,
) where

import Type.Domain (Condition (..))
import Type.Raw (RawCondition (..))

import Control.Monad (forM)
import qualified Control.Monad.Extra as E
import qualified Data.List.Extra as E
import Data.Maybe (fromMaybe)
import Data.Set (toList)
import Path.IO (AnyPath (makeAbsolute))
import Path.Posix (
    Abs,
    Dir,
    File,
    Path,
    PathException,
    Rel,
    SomeBase (Abs, Rel),
    parseRelDir,
    parseRelFile,
    parseSomeDir,
    toFilePath,
 )

import Control.Exception (catch, throwIO)
import Control.Monad.Extra (whenM)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import System.Directory (doesDirectoryExist)
import qualified System.Directory as D
import Text.Pretty.Simple (pPrintString, pString)
import Witch (into)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a : _) = Just a

-- Normalize functions

normalizeDirAbs :: Word -> FilePath -> IO (Path Abs Dir)
normalizeDirAbs index path = do
    let (homeMarker, relPath) = splitAt 1 path
    path' <- E.whenMaybe (homeMarker == "~") $ do
        home <- D.getHomeDirectory
        pure $ home <> "/" <> relPath
    someDir <- catch @PathException (parseSomeDir $ fromMaybe path path') $ \e -> do
        pPrintString $ "Target path " <> path <> " from condition " <> show index <> " is invalid"
        throwIO e
    aPath <- case someDir of
        Abs a -> pure a
        Rel r -> makeAbsolute r
    whenM (not <$> doesDirectoryExist (toFilePath aPath)) $
        throwIO $
            userError $
                into @String $
                    pString "Target path not found"
    pure aPath

normalizeRelFile :: FilePath -> IO (Path Rel File)
normalizeRelFile path = catch @PathException (parseRelFile path) $ \e -> do
    pPrintString $ "File path " <> path <> " is invalid"
    throwIO e

normalizeRelDir :: FilePath -> IO (Path Rel Dir)
normalizeRelDir path = catch @PathException (parseRelDir path) $ \e -> do
    pPrintString $ "Directory path " <> path <> " is invalid"
    throwIO e

prepareConditions
    :: [RawCondition]
    -> IO (Maybe (NonEmpty Condition))
prepareConditions raw = do
    conditions <- forM raw $ \RawCondition{..} -> do
        target <- normalizeDirAbs rcIndex $ E.trim rcTarget
        files <- mapM normalizeRelFile $ toList rcHasFiles
        dirs <- traverse normalizeRelDir $ toList rcHasDirectories
        let normalizedExt e = if "." == take 1 e then e else '.' : e
        let exts = map normalizedExt $ toList rcHasExtensions
        pure $
            Condition
                { cIndex = rcIndex
                , cTarget = target
                , cFiles = files
                , cDirectories = dirs
                , cExtensions = exts
                , cCommand = rcCommand
                , cArguments = rcArgs
                , cActConcurrent = rcActConcurrent
                , cWithBreak = rcWithBreak
                }
    pure $ nonEmpty conditions
