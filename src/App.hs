module App (
    appLineman,
) where

import Cook (safeHead)
import Lineman (launchAction)
import Log (mkLogEnv)
import Types (App (unApp), Config (..), Env (..))

import Control.Concurrent.Async.Lifted (forConcurrently)
import Control.Exception (throwIO, try, SomeException)
import Control.Monad (forM, when)
import Control.Monad.Reader (ReaderT (..))
import Dhall (auto, inputFile)
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
                            , envTarget = cTarget config
                            , envConditions = cConditions config
                            }
                runApp env launchAction

runApp :: Env -> App a -> IO a
runApp env app = runReaderT (unApp app) env

getConfig :: FilePath -> IO Config
getConfig path = do
    eConfig :: Either SomeException Config <- try $ inputFile auto path
    case eConfig of
        Left err -> do
            pPrintString "Config parsing failed"
            throwIO err
        Right decoded -> pure decoded
