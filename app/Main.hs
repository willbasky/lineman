module Main (main) where

import           Lineman (Config (..), launchActionInDirs)
import           Relude

main :: IO ()
main = launchActionInDirs conf

conf :: Config
conf = Config
  { taregetDirectory = "/home/metaxis/sources/Haskell/"
  , hasFiles         = ["stack.yaml"]
  , hasDirectories   = [".git", ".vscode"]
  , hasExtensions    = [".lock"] -- consume exts with and without '.'
  , command          = "pwd"
  , args             = []
  }
