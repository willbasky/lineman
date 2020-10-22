module Main (main) where

import Lineman (projectName)
import Relude

main :: IO ()
main = putText ("Executable for " <> projectName)
