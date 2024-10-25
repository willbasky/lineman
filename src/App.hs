module App (
    appLineman,
) where

import Colog (Msg (msgSeverity), filterBySeverity)
import Colog.Actions (richMessageAction, simpleMessageAction)
import Colog.Core (Severity (..))
import Config (Config (..), getConfig)
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
        Nothing -> pPrintString $ "Config file not found in " <> show mArgs
        Just path -> do
            config <- getConfig path
            pPrint config
            pPrintString "Launch command with that Config? (yes/no)"
            str <- getLine
            when (str == "yes") $ do
                let env1 = if cRichLog config then setRichLog defaultEnv else defaultEnv
                let env2 = setSeverity (cSeverity config) env1
                let env = if (cAsync config) then setAsyncMode env2 else env2
                runApp env $ launchAction config

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
