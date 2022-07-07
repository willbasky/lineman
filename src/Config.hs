module Config
       ( getConfig
       ) where


import           Data.Text          (unpack)
import           Text.Pretty.Simple (pPrintString)
import           Toml               (TomlCodec, (.=))
import qualified Toml
import           Types              (Config (..), ConfigElement (..))



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
    <*> Toml.set configElem "configElement" .= configElement

configElem :: TomlCodec ConfigElement
configElem = ConfigElement
    <$> Toml.arraySetOf Toml._String  "hasFiles"    .= hasFiles
    <*> Toml.arraySetOf Toml._String "hasDirectories" .= hasDirectories
    <*> Toml.arraySetOf Toml._String "hasExtensions" .= hasExtensions
    <*> Toml.string "command" .= command
    <*> Toml.arraySetOf Toml._String "args" .= args
