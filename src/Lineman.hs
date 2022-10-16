{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}


module Lineman
       ( launchAction
       ) where

import Colog (pattern D, pattern E, pattern I, log)
import Control.Concurrent.Async.Lifted ( forConcurrently )
import Control.Exception (try)
import Control.Monad (forM_)
import qualified Control.Monad.Extra as E
import Control.Monad.IO.Class (liftIO)
import Cooker (normailzeConfig)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.IO.Exception (ExitCode (..))
import Path.IO (doesDirExist, doesFileExist, listDir, listDirRecur, withCurrentDir)
import Path.Posix (Abs, Dir, File, Path, PathException, Rel, fileExtension, (</>))
import Prelude hiding (log)
import System.Process (createProcess, proc, waitForProcess)
import System.Process.Extra (showCommandForUser)
import Text.Pretty.Simple (pPrintString)
import Types ( App, Config )
import Control.Exception.Base (SomeException)


launchAction :: Config -> App ()
launchAction config = do
  list <- normailzeConfig config
  liftIO $ print list
  forM_ list $ \(mTarget, mFiles, dirs, exts, command, args) -> do
    dirsForLaunch <- case (mTarget, mFiles) of
      (Just t, Just fs) -> getDirsForCommand t fs dirs exts
      _                 -> pure []
    log D $ T.pack $ show dirsForLaunch
    codes <- seq dirsForLaunch $ forConcurrently dirsForLaunch $ \d -> do
    -- codes <- seq dirsForLaunch $ forM dirsForLaunch $ \d -> do
      let act = showCommandForUser command args
      let dir = T.pack (show d)
      log I $ "Action \'" <> T.pack act <> "\' is running at " <> dir
      withCurrentDir d $ action dir command args

    if all (== ExitSuccess) codes
      then pPrintString "All actions successfuly finished!"
      else pPrintString "Some action(s) failed"


getDirsForCommand :: Path Abs Dir -> [Path Rel File] -> [Path Rel Dir] -> [String] -> App [Path Abs Dir]
getDirsForCommand target files dirs exts = do
  (targets, _) <- listDirRecur target
  seq targets $ do
    res <- findDirsDyFiles (target : targets) files dirs exts
    log D $ "findDirsDyFiles: " <> T.pack (show res)
    pure res


action :: Text -> FilePath -> [String] -> App ExitCode
action dir commandName args = do
  (stin,stout,sterr,handleProc) <-
    liftIO $ createProcess $ proc commandName args
  log D $ dir <> " " <> "after proc"
  E.whenJust stin $ \x -> log D $ "stin: " <> T.pack (show x)
  E.whenJust stout $ \x -> log D $ "stout: " <> T.pack (show x)
  E.whenJust sterr $ \x -> log D $ "sterr: " <> T.pack (show x)
  log D $ dir <> " " <> "after when"
  res <- liftIO $ try $ waitForProcess handleProc
  case res of
    Left err -> do
      log E $ dir <> " " <> T.pack (show @SomeException err)
      pure $ ExitFailure 1
    Right r -> do
      log D $ dir <> " " <> T.pack (show r)
      pure r


findDirsDyFiles ::
  -- | Set of directories to search in
  [Path Abs Dir] ->
  -- | Set of filenames of interest
  [Path Rel File] ->
  -- | Set of directories of interest
  [Path Rel Dir] ->
  -- | Set of extensions of interest
  [String] ->
  -- | Absolute paths to all found files
  App [Path Abs Dir]
findDirsDyFiles [] _ _ _ = pure []
findDirsDyFiles d [] [] [] = pure d
findDirsDyFiles (d : ds) files dirs exts = do
  dFiles <- snd <$> listDir d
  existFiles <- E.allM (\f -> doesFileExist $ d </> f) files
  log D $ T.pack (show d)
  log D $ T.pack (show files)
  log D $ T.pack (show existFiles)
  existDirs <- E.allM (\f -> doesDirExist $ d </> f) dirs
  existExts <- isExtsInFiles exts dFiles
  if existFiles && existDirs && existExts
    then (d :) <$> findDirsDyFiles ds files dirs exts
    else findDirsDyFiles ds files dirs exts


isExtsInFiles :: [String] -> [Path Abs File] -> App Bool
isExtsInFiles exts files = liftIO $ flip E.allM exts $ \ext -> do
  flip E.anyM files $ \df -> do
    hasExt <- try $ fileExtension df
    pure $ case hasExt of
      Left (_ :: PathException) -> False
      Right fExt                -> fExt == ext
