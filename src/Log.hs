{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Log (
    withLogEnv,
    mkLogEnv,
    logInfo,
    logDebug,
    logError,
    logWarn,
    logInfoS,
    logDebugS,
    logErrorS,
    logWarnS,

) where

import Control.Exception.Safe (bracket)
import Data.Text (Text)
import Data.Text.Lazy.Builder (fromString, fromText)
import Katip
import Katip.Core (intercalateNs, locationToString)
import Katip.Format.Time (formatAsIso8601, formatAsLogTime)
import Katip.Scribes.Handle (brackets, getKeys)
import System.IO (stdout)
import Text.Pretty.Simple (pShow)

-- Verbosity 0 show time, severity and message
-- Verbosity 1 severity, message and time with picoseconds
-- Verbosity 2 like V1 plus log location
-- Verbosity 3 like V2 plus PID and ThreadId
logFormatter :: (LogItem a) => ItemFormatter a
logFormatter withColor verb Item{..} =
    brackets nowStr
        <> brackets (mconcat $ map fromText $ intercalateNs _itemNamespace)
        <> brackets (fromText (renderSeverity' _itemSeverity))
        <> verbMore
            V2
            mempty
            ( brackets ("PID" <> " " <> fromString (show _itemProcess))
                <> brackets ("ThreadId" <> " " <> fromText (getThreadIdText _itemThread))
            )
        <> mconcat ks
        <> verbMore V1 mempty (maybe mempty (brackets . fromString . locationToString) _itemLoc)
        <> fromText " "
        <> (unLogStr _itemMessage)
  where
    nowStr = fromText (verbMore V0 formatAsLogTime formatAsIso8601 $ _itemTime)
    ks = map brackets $ getKeys verb _itemPayload
    verbMore v def t = if verb > v then t else def
    renderSeverity' severity =
        colorBySeverityFull withColor severity (renderSeverity severity)

withLogEnv :: IO LogEnv -> (LogEnv -> IO ()) -> IO ()
withLogEnv logEnv = bracket logEnv closeScribes

mkLogEnv
    :: Verbosity
    -> Severity
    -> IO LogEnv
mkLogEnv verbosity severity = do
    handleScribe <-
        mkHandleScribeWithFormatter
            logFormatter
            ColorIfTerminal
            stdout
            (permitItem severity)
            verbosity
    initiatedLogEnv <- initLogEnv "Hibet" (Environment "prod")
    registerScribe
        "stdout"
        handleScribe
        defaultScribeSettings
        initiatedLogEnv

logWarn :: (KatipContext m) => Text -> m ()
logWarn = logLocM WarningS . ls

logInfo :: (KatipContext m) => Text -> m ()
logInfo = logLocM InfoS . ls

logError :: (KatipContext m) => Text -> m ()
logError = logLocM ErrorS . ls

logDebug :: (KatipContext m) => Text -> m ()
logDebug = logLocM DebugS . ls

-- The following logs colorize and format Haskell types.
logWithFormat :: (Show a, KatipContext m) => Severity -> Bool -> a -> m ()
logWithFormat severity withFormat = logLocM severity . format
  where
    format = if withFormat then (ls . pShow) else showLS

logWarnS :: (Show a, KatipContext m) => Bool -> a -> m ()
logWarnS = logWithFormat WarningS

logInfoS :: (Show a, KatipContext m) => Bool -> a -> m ()
logInfoS = logWithFormat InfoS

logErrorS :: (Show a, KatipContext m) => Bool -> a -> m ()
logErrorS = logWithFormat ErrorS

logDebugS :: (Show a, KatipContext m) => Bool -> a -> m ()
logDebugS = logWithFormat DebugS

colorBySeverityFull :: Bool -> Severity -> Text -> Text
colorBySeverityFull withColor severity msg = case severity of
    EmergencyS -> red msg
    AlertS -> red msg
    CriticalS -> red msg
    ErrorS -> red msg
    WarningS -> yellow msg
    InfoS -> green msg
    DebugS -> blue msg
    _ -> msg
  where
    red = colorize "31"
    green = colorize "32"
    yellow = colorize "33"
    blue = colorize "34"
    colorize c s
        | withColor = "\ESC[" <> c <> "m" <> s <> "\ESC[0m"
        | otherwise = s
