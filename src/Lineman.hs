{-# LANGUAGE TypeApplications #-}

module Lineman (
    launchAction,
) where

import Cook (prepareConditions)
import Log (logDebug, logError, logInfo)
import Types (App, Env (..))

import Control.Exception.Safe (try)
import Control.Monad (forM_)
import qualified Control.Monad.Extra as E
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import qualified Data.Text as T
import GHC.IO.Exception (ExitCode (..))
import Path.IO (doesDirExist, doesFileExist, listDir, listDirRecur, withCurrentDir)
import Path.Posix (Abs, Dir, File, Path, PathException, Rel, fileExtension, (</>))
import System.Process (proc, readCreateProcessWithExitCode)
import System.Process.Extra (showCommandForUser)
import Prelude hiding (log)

launchAction :: App ()
launchAction = do
    target <- asks envTarget
    conditions <- asks envConditions
    list <- prepareConditions target conditions
    logDebug $ T.pack $ show list
    forM_ list $ \(mTarget, mFiles, dirs, exts, command, args) -> do
        dirsForLaunch <- case (mTarget, mFiles) of
            (Just t, Just fs) -> getDirsForCommand t fs dirs exts
            _ -> pure []
        logDebug $ T.pack $ show dirsForLaunch
        forAction <- asks envActionMode
        codes <- seq dirsForLaunch $
            forAction dirsForLaunch $ \d -> do
                let act = showCommandForUser command args
                let dir = T.pack (show d)
                logInfo $ "Action \'" <> T.pack act <> "\' is running in " <> dir
                withCurrentDir d $ action command args
        if all (== ExitSuccess) codes
            then logInfo "All actions successfuly finished!"
            else logError "Some action(s) failed"

getDirsForCommand :: Path Abs Dir -> [Path Rel File] -> [Path Rel Dir] -> [String] -> App [Path Abs Dir]
getDirsForCommand target files dirs exts = do
    (targets, _) <- listDirRecur target
    seq targets $ do
        res <- findDirsDyFiles (target : targets) files dirs exts
        logDebug $ "findDirsDyFiles: " <> T.pack (show res)
        pure res

action :: FilePath -> [String] -> App ExitCode
action commandName args = do
    (exitCode, stdout, stderr) <-
        liftIO $
            readCreateProcessWithExitCode (proc commandName args) ""
    case stderr of
        "" -> pure ()
        err -> logError $ "stderr: " <> T.strip (T.pack err)
    case stdout of
        "" -> pure ()
        out -> logDebug $ "stdout: " <> T.pack out
    logDebug $ T.pack (show exitCode)
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
    logDebug $ T.pack (show d)
    logDebug $ T.pack (show files)
    logDebug $ T.pack (show existFiles)
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
