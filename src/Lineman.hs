{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Lineman (
    launchAction,
)
where

import Concurrent (forConcurrentlyKi, forConcurrentlyKi_)
import Type.Domain (App, Condition (..), Env (..))

-- import Control.Concurrent (threadDelay)
import Control.Exception.Safe (try)
import Control.Monad (forM, forM_)
import qualified Control.Monad.Extra as E
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Text (Text)
import qualified Data.Text as T
import Log (logDebug, logError, logInfo)
import Path.IO (doesDirExist, doesFileExist, listDir, listDirRecur)
import Path.Posix (Abs, Dir, File, Path, PathException, Rel, fileExtension, toFilePath, (</>))
import System.Process.Extra (showCommandForUser)
import System.Process.Typed
import Witch
import Prelude hiding (log)

launchAction :: App ()
launchAction = do
    env <- ask
    let conditions = envConditions env
    logDebug $ "Conditions: " <> into @Text (show conditions)
    let forSwarm = if envSwarmConcurrent env then forConcurrentlyKi_ else forM_
    forSwarm conditions $ \Condition{..} -> do
        dirsForLaunch <- case (cTarget, cFiles) of
            (Just target, Just files) -> getDirsForCommand target files cDirectories cExtensions
            _ -> pure []
        logDebug $ "Directories for running action: " <> into @Text (show dirsForLaunch)
        let forAction = if cActConcurrent then forConcurrentlyKi else forM
        codes <- seq dirsForLaunch $
            forAction dirsForLaunch $ \d -> do
                let act = showCommandForUser cCommand cArguments
                let dir = into @Text (show d)
                logInfo $ "Action \'" <> into @Text act <> "\' is running in " <> dir
                action cCommand cArguments d
        if all (== ExitSuccess) codes
            then logInfo "All actions successfuly finished!"
            else logError "Some action(s) failed"

getDirsForCommand
    :: Path Abs Dir
    -> [Path Rel File]
    -> [Path Rel Dir]
    -> [String]
    -> App [Path Abs Dir]
getDirsForCommand target files dirs exts = do
    (targets, _) <- listDirRecur target
    seq targets $ do
        res <- findDirsDyFiles (target : targets) files dirs exts
        logDebug $ "Found directories: " <> into @Text (show res)
        pure res

action :: FilePath -> [String] -> Path Abs Dir -> App ExitCode
action commandName args path = do
    -- liftIO $ threadDelay 500_000 -- 0.5 seconds
    let dateConfig :: ProcessConfig () () ()
        dateConfig = setWorkingDir (toFilePath path) $ proc commandName args
    (exitCode, stdout, stderr) <-
        liftIO $
            readProcess dateConfig
    case stderr of
        "" -> pure ()
        err ->
            logError $
                "In "
                    <> into @Text (toFilePath path)
                    <> " occurred stderr: \n"
                    <> T.strip (unsafeInto @Text $ into @Utf8L err)
    case stdout of
        "" -> pure ()
        out ->
            logDebug $
                "In "
                    <> into @Text (toFilePath path)
                    <> " occurred stdout: \n"
                    <> unsafeInto @Text (into @Utf8L out)
    logDebug $ into @Text (show exitCode)
    pure exitCode

findDirsDyFiles
    :: [Path Abs Dir]
    -- ^ Set of directories to search in
    -> [Path Rel File]
    -- ^ Set of filenames of interest
    -> [Path Rel Dir]
    -- ^ Set of directories of interest
    -> [String]
    -- ^ Set of extensions of interest
    -> App [Path Abs Dir]
    -- ^ Absolute paths to all found files
findDirsDyFiles [] _ _ _ = pure []
findDirsDyFiles d [] [] [] = pure d
findDirsDyFiles (d : ds) files dirs exts = do
    dFiles <- snd <$> listDir d
    existFiles <- E.allM (\f -> doesFileExist $ d </> f) files
    logDebug $ "In directory: " <> into @Text (toFilePath d)
    logDebug $ "file(s) " <> into @Text (show files)
    logDebug $ "exist? " <> into @Text (show existFiles)
    existDirs <- E.allM (\f -> doesDirExist $ d </> f) dirs
    existExts <- isExtsInFiles exts dFiles
    if existFiles && existDirs && existExts
        then (d :) <$> findDirsDyFiles ds files dirs exts
        else findDirsDyFiles ds files dirs exts

isExtsInFiles :: [String] -> [Path Abs File] -> App Bool
isExtsInFiles exts files = liftIO $
    flip E.allM exts $ \ext -> do
        flip E.anyM files $ \df -> do
            hasExt <- try $ fileExtension df
            pure $ case hasExt of
                Left (_ :: PathException) -> False
                Right fExt -> fExt == ext
