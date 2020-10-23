module Main (main) where

import Lineman (launchActionInDirs, Config(..))
import Relude

main :: IO ()
main = launchActionInDirs conf

conf :: Config
conf = Config
  { directory = "/home/metaxis/sources/Haskell/"
  , files     = ["stack.yaml"]
  , command   = "stack"
  , args      = ["clean"]
  }
