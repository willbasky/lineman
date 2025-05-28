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
    parseSomeDir,
    parseSomeFile,
 )

import Control.Exception (catch, throwIO)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified System.Directory as D
import qualified System.FilePath.Posix as FP
import Text.Pretty.Simple (pPrintString)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a : _) = Just a

-- Normalize functions

normalizeDirAbs :: FilePath -> IO (Path Abs Dir)
normalizeDirAbs path = do
    let (homeMarker, relPath) = splitAt 1 path
    path' <- E.whenMaybe (homeMarker == "~") $ do
        home <- D.getHomeDirectory
        pure $ home <> "/" <> relPath
    someDir <- parseSomeDir $ fromMaybe path path'
    case someDir of
        Abs a -> pure a
        Rel r -> makeAbsolute r

normalizeFile :: FilePath -> IO (Maybe (Path Rel File))
normalizeFile path =
    if FP.isRelative path && FP.isValid path && not (FP.hasTrailingPathSeparator path)
        then do
            someFile <- parseSomeFile path
            case someFile of
                Abs _ -> pure Nothing
                Rel r -> pure $ Just r
        else pure Nothing

prepareConditions
    :: [RawCondition]
    -> IO (Maybe (NonEmpty Condition))
prepareConditions raw = do
    conditions <- forM raw $ \RawCondition{..} -> do
        target <- catch @PathException (normalizeDirAbs $ E.trim rcTarget) $ \e -> do
            pPrintString $ "Target path in condition " <> show rcIndex <> " is invalid"
            throwIO e
        mFiles <- sequence <$> traverse normalizeFile (toList rcHasFiles)
        dirs <- traverse parseRelDir $ toList rcHasDirectories
        let normalizedExt e = if "." == take 1 e then e else '.' : e
        let exts = map normalizedExt $ toList rcHasExtensions
        pure $
            Condition
                { cIndex = rcIndex
                , cTarget = target
                , cFiles = mFiles
                , cDirectories = dirs
                , cExtensions = exts
                , cCommand = rcCommand
                , cArguments = rcArgs
                , cActConcurrent = rcActConcurrent
                , cWithBreak = rcWithBreak
                }
    pure $ nonEmpty conditions
