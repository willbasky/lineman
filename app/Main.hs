module Main (main) where

import           Config             (getConfig)
import           Cooker             (safeHead)
import           Lineman            (launchAction)
import           System.Environment (getArgs)
import           Text.Pretty.Simple (pPrint, pPrintString)

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
        "yes" -> launchAction config
        _ -> pPrintString "... then bye"

