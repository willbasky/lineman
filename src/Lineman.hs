module Lineman
       ( launchAction
       ) where

import           Control.Exception   (try)
import           Control.Monad       (forM)
import qualified Control.Monad.Extra as E
import           Path.IO
import           Path.Posix
import           System.Exit
import           System.Process
import           Text.Pretty.Simple  (pPrintString)

import           Cooker              (normailzeConfig)
import           Types               (Config (..))




launchAction :: Config -> IO ()
launchAction config@Config{..} = do
  (mTarget, mFiles, dirs, exts) <- normailzeConfig config
  dirsForLaunch <- case (mTarget, mFiles) of
    (Just t, Just fs) -> getDirsForCommand t fs dirs exts
    _                 -> pure []
  codes <- seq dirsForLaunch $ forM dirsForLaunch $ \d -> do
    pPrintString $ "Action is running at " <> show d
    withCurrentDir d $ action command args
  -- for debuging

    -- code <- withCurrentDir d $ action command args
    -- case code of
    --   ExitSuccess   -> putStrLn "Action successfuly finished!"
    --   ExitFailure n -> putStrLn $ "Action failed with code " <> show n
    -- pure code

  if all (== ExitSuccess) codes
    then pPrintString "All actions successfuly finished!"
    else pPrintString "Some action(s) failed"

  -- mapM_ print dirs


getDirsForCommand :: Path Abs Dir -> [Path Rel File] -> [Path Rel Dir] -> [String] -> IO [Path Abs Dir]
getDirsForCommand target files dirs exts = do
  (targets, _) <- listDirRecur target
  seq targets $ findDirsDyFiles (target : targets) files dirs exts


action :: FilePath -> [String] -> IO ExitCode
action commandName args = do
  (stin,stout,sterr,handleProc) <- createProcess $ proc commandName args
  E.whenJust stin print
  E.whenJust stout print
  E.whenJust sterr print
  waitForProcess handleProc


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
  IO [Path Abs Dir]
findDirsDyFiles [] _ _ _ = pure []
findDirsDyFiles d [] [] [] = pure d
findDirsDyFiles (d : ds) files dirs exts = do
  dFiles <- snd <$> listDir d
  existFiles <- E.allM (\f -> doesFileExist $ d </> f) files
  existDirs <- E.allM (\f -> doesDirExist $ d </> f) dirs
  existExts <- isExtsInFiles exts dFiles
  if existFiles && existDirs && existExts
    then (d :) <$> findDirsDyFiles ds files dirs exts
    else findDirsDyFiles ds files dirs exts


isExtsInFiles :: [String] -> [Path Abs File] -> IO Bool
isExtsInFiles exts files = flip E.allM exts $ \ext -> do
  flip E.anyM files $ \df -> do
    hasExt <- try $ fileExtension df
    pure $ case hasExt of
      Left (_ :: PathException) -> False
      Right fExt                -> fExt == ext
