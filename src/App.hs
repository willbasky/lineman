module App (
    appLineman,
) where

import Lineman (launchAction)
import Log (mkLogEnv)
import Parser (prepareConditions, safeHead)
import Type.Domain (App (unApp), Env (..))
import Type.Raw (Config (..))

import Control.Exception.Safe (throwIO, tryAny)
import Control.Monad (when)
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
                conditions <- prepareConditions $ confRawConditions config
                logEnv <- mkLogEnv (confVerbosity config) (confSeverity config)
                let env =
                        Env
                            { envLogEnv = logEnv
                            , envLogContext = mempty
                            , envLogNamespace = mempty
                            , envConditions = conditions
                            , envSwarmConcurrent = confSwarmConcurrent config
                            }
                runApp env launchAction

runApp :: Env -> App a -> IO a
runApp env app = runReaderT (unApp app) env

getConfig :: FilePath -> IO Config
getConfig path = do
    eConfig :: Either a Config <- tryAny $ inputFile auto path
    case eConfig of
        Left err -> do
            pPrintString "Config parsing failed"
            throwIO err
        Right decoded -> pure decoded
