{-# LANGUAGE DerivingStrategies #-}

module Lineman
       (
         launchActionInDirs
       , Config(..)
       ) where

import           Relude                hiding (unlessM)

import           Control.Exception     (try)
import qualified Control.Monad.Extra as E
import qualified Data.List.Extra as E
import           Path.IO
import           Path.Posix
import qualified System.Directory      as D
import           System.Exit
import qualified System.FilePath.Posix as FP
import           System.Process



data Config = Config
  { taregetDirectory :: FilePath
  , hasFiles         :: [FilePath]
  , hasDirectories   :: [FilePath]
  , hasExtensions    :: [String]
  , command          :: String
  , args             :: [String]
  }
  deriving stock (Show, Eq)

data FoundPath = FoundPath
  { foundPath :: ![Path Abs Dir]
  }
  deriving stock (Show, Eq)

launchActionInDirs :: Config -> IO ()
launchActionInDirs Config{..} = do
  dirs <- normailzeAndGetDirs taregetDirectory hasFiles hasDirectories hasExtensions
  codes <- seq dirs $ forM dirs $ \d -> do
    putTextLn $ "Action is running at " <> show d
    withCurrentDir d $ action command args
  -- for debuging

    -- code <- withCurrentDir d $ action command args
    -- case code of
    --   ExitSuccess   -> putTextLn "Action successfuly finished!"
    --   ExitFailure n -> putTextLn $ "Action failed with code " <> show n
    -- pure code


  if all (== ExitSuccess) codes
    then putTextLn "All actions successfuly finished!"
    else putTextLn "Some action(s) failed"

  -- mapM_ print dirs

normailzeAndGetDirs :: FilePath -> [FilePath] -> [FilePath] -> [String] -> IO [Path Abs Dir]
normailzeAndGetDirs pathTarget pathFiles pathDirs exts = do
  mTarget <- normilizeDirAbs $ E.trim pathTarget -- "/home/metaxis/sources/Haskell/"
  mFiles <- sequence <$> traverse normilizeFile pathFiles -- ["stack.yaml", "README.md"]
  dirs <- traverse normilizeDirRel pathDirs
  case (mTarget, mFiles) of
    (Just t, Just fs) -> getDirsForCommand t fs dirs exts
    _                 -> pure []

getDirsForCommand :: Path Abs Dir -> [Path Rel File] -> [Path Rel Dir] -> [String] -> IO [Path Abs Dir]
getDirsForCommand target files dirs exts = do
  (targets, _) <- listDirRecur target
  seq targets $ findDirsDyFiles (target : targets) files dirs exts


normilizeDirAbs :: FilePath -> IO (Maybe (Path Abs Dir))
normilizeDirAbs path = do
  let (homeMarker, relPath) = splitAt 1 path
  path' <- E.whenMaybe (homeMarker == "~" ) $ do
    home <- D.getHomeDirectory
    pure $ home <> "/" <> relPath
  someDir <- parseSomeDir $ fromMaybe path path'
  case someDir of
    Abs a -> pure $ Just a
    Rel r -> Just <$> makeAbsolute r

normilizeDirRel :: FilePath -> IO (Path Rel Dir)
normilizeDirRel = parseRelDir


normilizeFile :: FilePath -> IO (Maybe (Path Rel File))
normilizeFile path =
  if FP.isRelative path && FP.isValid path && not (FP.hasTrailingPathSeparator path)
    then do
      someFile <- parseSomeFile path
      case someFile of
        Abs _ -> pure Nothing
        Rel r -> pure $ Just r
    else pure Nothing


action :: FilePath -> [String] -> IO ExitCode
action commandName args = do
  (stin,stout,sterr,handleProc) <- createProcess $ proc commandName args
  whenJust stin print
  whenJust stout print
  whenJust sterr print
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
  existFiles <- allM (\f -> doesFileExist $ d </> f) files
  existDirs <- allM (\f -> doesDirExist $ d </> f) dirs
  existExts <- isExtsInFiles exts dFiles
  if existFiles && existDirs && existExts
    then (d :) <$> findDirsDyFiles ds files dirs exts
    else findDirsDyFiles ds files dirs exts


isExtsInFiles :: [String] -> [Path Abs File] -> IO Bool
isExtsInFiles exts files = flip allM exts $ \e -> do
  let normalizedExt = if "." == take 1 e then e else '.' : e
  flip anyM files $ \df -> do
    hasExt <- try $ fileExtension df
    pure $ case hasExt of
      Left (_ :: PathException) -> False
      Right fExt                -> fExt == normalizedExt
