module App (
    appLineman,
) where

import Config (Config (..), getConfig)
import Cook (safeHead)
import Lineman (launchAction)
import Log (mkLogEnv)
import Types (App (unApp), Env (..))

import Control.Concurrent.Async.Lifted (forConcurrently)
import Control.Monad (forM, when)
import Control.Monad.Reader (ReaderT (..))
import System.Environment (getArgs)
import Text.Pretty.Simple (pPrint, pPrintString)

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
                logEnv <- mkLogEnv (cVerbosity config) (cSeverity config)
                let env =
                        Env
                            { envLogEnv = logEnv
                            , envActionMode =
                                if cAsync config
                                    then forConcurrently
                                    else forM
                            , envLogContext = mempty
                            , envLogNamespace = mempty
                            }
                runApp env $ launchAction config

runApp :: Env -> App a -> IO a
runApp env app = runReaderT (unApp app) env
