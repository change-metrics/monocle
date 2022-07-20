-- | The `cabal test` entry point
module Main (main) where

import Monocle.Prelude
import Tests qualified

main :: IO ()
main = Tests.main
