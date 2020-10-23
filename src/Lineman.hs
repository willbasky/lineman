{-# LANGUAGE DerivingStrategies #-}

module Lineman
       (
         launchActionInDirs
       , Config(..)
       ) where

import           Relude                hiding (unlessM)

import           Path.IO
import           Path.Posix
import qualified System.Directory      as D
import           System.Exit
import qualified System.FilePath.Posix as FP
import           System.Process



data Config = Config
  { directory :: FilePath
  , files     :: [FilePath]
  , command   :: String
  , args      :: [String]
  }
  deriving stock (Show, Eq)

launchActionInDirs :: Config -> IO ()
launchActionInDirs Config{..} = do
  dirs <- normailzeAndGetDirs directory files
  codes <- forM dirs $ \d -> do
    putTextLn $ "Action is running at " <> show d
    withCurrentDir d $ action command args
    -- code <- withCurrentDir d $ action command args
    -- case code of
    --   ExitSuccess   -> putTextLn "Action successfuly finished!"
    --   ExitFailure n -> putTextLn $ "Action failed with code " <> show n
    -- pure code
  if all (== ExitSuccess) codes
    then putTextLn "All actions successfuly finished!"
    else putTextLn "Some action(s) failed"
  -- mapM_ print dirs

normailzeAndGetDirs :: FilePath -> [FilePath] -> IO [Path Abs Dir]
normailzeAndGetDirs pathD pathFs = do
  dir <- normilizeDir pathD -- "/home/metaxis/sources/Haskell/"
  files <- sequence <$> traverse normilizeFile pathFs -- ["stack.yaml", "README.md"]
  case (dir, files) of
    (Just d, Just fs) -> getDirsForCommand d fs
    _                 -> pure []

getDirsForCommand :: Path Abs Dir -> [Path Rel File] -> IO [Path Abs Dir]
getDirsForCommand dir preds = do
  (dirs, _) <- listDirRecur dir
  findDirsDyFiles (dir : dirs) preds


normilizeDir :: FilePath -> IO (Maybe (Path Abs Dir))
normilizeDir path = do
  canPath <- D.canonicalizePath path
  ifM (D.doesDirectoryExist canPath) (makeAbs canPath) (pure Nothing)
  where
    makeAbs canPath = do
      someDir <- parseSomeDir canPath
      case someDir of
        Abs a -> pure $ Just a
        Rel r -> Just <$> makeAbsolute r

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
  (_,_,_,handleProc) <- createProcess $ proc commandName args
  waitForProcess handleProc


findDirsDyFiles ::
  MonadIO m =>
  -- | Set of directories to search in
  [Path Abs Dir] ->
  -- | Set of filename of interest
  [Path Rel File] ->
  -- | Absolute paths to all found files
  m [Path Abs Dir]
findDirsDyFiles [] _ = pure []
findDirsDyFiles _ [] = pure []
findDirsDyFiles (d : ds) files = do
  exist <- allM (\f -> doesFileExist $ d </> f) files
  if exist
    then (d :) <$> findDirsDyFiles ds files
    else findDirsDyFiles ds files
