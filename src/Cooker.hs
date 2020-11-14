module Cooker
  ( safeHead
  , normailzeConfig
  ) where

import qualified Control.Monad.Extra   as E
import qualified Data.List.Extra       as E
import           Path.IO
import           Path.Posix
import qualified System.Directory      as D
import qualified System.FilePath.Posix as FP
import           Data.Maybe            (fromMaybe)

import           Types                 (Config (..))



safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (a:_) = Just a


-- Normilize functions

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


normailzeConfig
  :: Config
  -> IO (Maybe (Path Abs Dir), Maybe [Path Rel File], [Path Rel Dir], [String])
normailzeConfig Config{..} = do
  mTarget <- normilizeDirAbs $ E.trim taregetDirectory
  mFiles <- sequence <$> traverse normilizeFile hasFiles
  dirs <- traverse normilizeDirRel hasDirectories
  let normalizedExt e = if "." == take 1 e then e else '.' : e
  let exts = map normalizedExt hasExtensions
  pure (mTarget, mFiles, dirs, exts)
