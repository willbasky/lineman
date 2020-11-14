module Config
       ( getConfig
       ) where


import           Data.Text          (unpack)
import           Text.Pretty.Simple (pPrintString)
import           Toml               (TomlCodec, (.=))
import qualified Toml
import           Types              (Config (..))



getConfig :: FilePath -> IO Config
getConfig file = do
  toml <- Toml.decodeFileEither configCodec file
  case toml of
    Left err      -> do
      pPrintString "Toml parsing failed"
      error . unpack $ Toml.prettyTomlDecodeErrors err
    Right decoded -> pure decoded

configCodec :: TomlCodec Config
configCodec = Config
    <$> Toml.string "taregetDirectory"  .= taregetDirectory
    <*> Toml.arrayOf Toml._String  "hasFiles"    .= hasFiles
    <*> Toml.arrayOf Toml._String "hasDirectories" .= hasDirectories
    <*> Toml.arrayOf Toml._String "hasExtensions" .= hasExtensions
    <*> Toml.string "command" .= command
    <*> Toml.arrayOf Toml._String "args" .= args
