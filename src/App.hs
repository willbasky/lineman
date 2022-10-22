module App
  ( appLineman
  ) where

import Colog (Msg (msgSeverity), filterBySeverity)
import Colog.Actions (richMessageAction, simpleMessageAction)
import Colog.Core (Severity (..))
import Config (getConfig)
import Control.Monad.Reader (ReaderT (..))
import Cooker (safeHead)
import Data.Char (toLower, toUpper)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Lineman (launchAction)
import System.Environment (getArgs, getEnvironment)
import Text.Pretty.Simple (pPrint, pPrintString)
import Text.Read (readMaybe)
import Types (App (unApp), Env (..))


appLineman :: IO ()
appLineman = do
  mArgs <- safeHead <$> getArgs
  case mArgs of
    Nothing -> pPrintString "No toml's config path found in args"
    Just path -> do
      threshold <- getEnvSeverity
      pPrintString $ "Severity: " <> show threshold
      config <- getConfig path
      pPrint config
      pPrintString "Launch command with that Config? (yes/no)"
      str <- getLine
      case str of
        "yes" -> runApp (setSeverity threshold) $ launchAction config
        _     -> pPrintString "... then bye"

runApp :: Env App -> App a -> IO a
runApp env app = runReaderT (unApp app) env

setSeverity :: Severity -> Env App
setSeverity threshold
  = Env
  $ filterBySeverity threshold msgSeverity
  $ envLogAction (simpleEnv threshold)

simpleEnv :: Severity -> Env App
simpleEnv s = Env
    { envLogAction  = case s of
        Debug -> richMessageAction
        _     -> simpleMessageAction
    }

-- Get LINEMAN_SEVERITY envvar
environmentVars :: IO (Map String String)
environmentVars = Map.fromList <$> getEnvironment

lookupEnv :: String -> IO (Maybe String)
lookupEnv k = Map.lookup k <$> environmentVars

getEnvSeverity :: IO Severity
getEnvSeverity = do
  isDebug <- lookupEnv "LINEMAN_SEVERITY"
  pure $ fromMaybe Error (readMaybe . toTitle =<< isDebug)

toTitle :: String -> String
toTitle ""     = ""
toTitle (x:xs) = toUpper x : map toLower xs


