module Main (main) where

import           Lineman (Config (..), launchActionInDirs)
import           Relude

main :: IO ()
main = launchActionInDirs conf

conf :: Config
conf = Config
  { taregetDirectory = "~/Documents/test"
  , hasFiles         = []
  , hasDirectories   = []
  , hasExtensions    = [] -- consume exts with and without '.'
  , command          = "pwd"
  , args             = []
  }
