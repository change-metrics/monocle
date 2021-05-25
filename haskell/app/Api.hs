{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | The monocle-api entrypoint
module Main (main) where

import Data.Text.IO (hPutStrLn)
import qualified Monocle.WebServerApi
import Relude

main :: IO ()
main = do
  args <- getArgs
  config <- fromMaybe "/etc/monocle/config.yaml" <$> lookupEnv "CONFIG"
  case args of
    ["--port", portStr] ->
      let port = fromMaybe (error $ "Could not read port: " <> toText portStr) $ readMaybe portStr
       in Monocle.WebServerApi.main port config
    _ -> hPutStrLn stderr "usage: monocle-api --port PORT"
