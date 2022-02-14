module Main (main) where

import Monocle.Prelude
import qualified Monocle.Test.Spec

main :: IO ()
main = withOpenSSL $ Monocle.Test.Spec.main
