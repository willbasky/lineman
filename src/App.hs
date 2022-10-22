module App (
  appLineman,
) where

import Colog (Msg (msgSeverity), filterBySeverity)
import Colog.Actions (richMessageAction, simpleMessageAction)
import Colog.Core (Severity (..))
import Config (getConfig)
import Control.Concurrent.Async.Lifted (forConcurrently)
import Control.Monad (forM, when)
import Control.Monad.Reader (ReaderT (..))
import Data.Char (toLower, toUpper)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import System.Environment (getArgs, getEnvironment)
import Text.Pretty.Simple (pPrint, pPrintString)
import Text.Read (readMaybe)

import Cook (safeHead)
import Lineman (launchAction)
import Types (App (unApp), Env (..))

appLineman :: IO ()
appLineman = do
  mArgs <- safeHead <$> getArgs
  case mArgs of
    Nothing -> pPrintString "No toml's config path found in args"
    Just path -> do
      config <- getConfig path
      pPrint config
      pPrintString "Launch command with that Config? (yes/no)"
      str <- getLine
      when (str == "yes") $ do
        rich <- getRichEnv
        let env1 = if rich then setRichLog defaultEnv else defaultEnv

        threshold <- getSeverityEnv
        let env2 = setSeverity threshold env1

        asyncMode <- getAsyncEnv
        let env3 = if asyncMode then setAsyncMode env2 else env2

        pPrintString ""
        when asyncMode $ pPrintString "[ Async mode ]"
        when rich $ pPrintString "[ Rich logs ]"
        pPrintString $ "[ " <> show threshold <> " severity ]"
        pPrintString ""

        runApp env3 $ launchAction config

runApp :: Env App -> App a -> IO a
runApp env app = runReaderT (unApp app) env

setSeverity :: Severity -> Env App -> Env App
setSeverity threshold env =
  env{envLogAction = filterBySeverity threshold msgSeverity $ envLogAction env}

setAsyncMode :: Env App -> Env App
setAsyncMode env =
  env{actionMode = forConcurrently}

setRichLog :: Env App -> Env App
setRichLog env =
  env{envLogAction = richMessageAction}

defaultEnv :: Env App
defaultEnv =
  Env
    { envLogAction = simpleMessageAction
    , actionMode = forM
    }

-- Get LINEMAN_SEVERITY envvar
getSeverityEnv :: IO Severity
getSeverityEnv = do
  isDebug <- lookupEnv "LINEMAN_SEVERITY"
  pure $ fromMaybe Error (readMaybe . toTitle =<< isDebug)

getAsyncEnv :: IO Bool
getAsyncEnv = do
  isDebug <- lookupEnv "LINEMAN_ASYNC"
  pure $ Just True == (readMaybe . toTitle =<< isDebug)

getRichEnv :: IO Bool
getRichEnv = do
  isDebug <- lookupEnv "LINEMAN_RICH_LOG"
  pure $ Just True == (readMaybe . toTitle =<< isDebug)

-- Help functions

environmentVars :: IO (Map String String)
environmentVars = Map.fromList <$> getEnvironment

lookupEnv :: String -> IO (Maybe String)
lookupEnv k = Map.lookup k <$> environmentVars

toTitle :: String -> String
toTitle "" = ""
toTitle (x : xs) = toUpper x : map toLower xs
