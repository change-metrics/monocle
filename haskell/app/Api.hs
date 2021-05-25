-- | The monocle-api entrypoint
module Main (main) where

import qualified Monocle.WebServerApi

main :: IO ()
main = Monocle.WebServerApi.main
