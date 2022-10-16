module Main (main) where

import Config (getConfig)
import Cooker (safeHead)
import Lineman (launchAction)
import System.Environment (getArgs)
import Text.Pretty.Simple (pPrint, pPrintString)
import Types ( App(unApp), Env(..) )
import Control.Monad.Reader (ReaderT (..))
import Colog.Actions


main :: IO ()
main = do
  mArgs <- safeHead <$> getArgs
  case mArgs of
    Nothing -> pPrintString "No toml's config path found in args"
    Just path -> do
      config <- getConfig path
      pPrint config
      pPrintString "Launch command with that Config? (yes/no)"
      str <- getLine
      case str of
        -- "yes" -> runApp simpleEnv $ launchAction config
        "yes" -> runApp simpleEnv $ launchAction config
        _     -> pPrintString "... then bye"

simpleEnv :: Env App
simpleEnv = Env
    { envLogAction  = richMessageAction
    -- { envLogAction  = simpleMessageAction
    }

runApp :: Env App -> App a -> IO a
runApp env app = runReaderT (unApp app) env
