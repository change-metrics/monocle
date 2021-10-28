{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Main (main) where

import Macroscope.Main (runMacroscope)
import Monocle.Client (withClient)
import Options.Generic
import Relude

data Macroscope w = Macroscope
  { monocleUrl :: w ::: Maybe Text <?> "The monocle API",
    port :: w ::: Int <!> "9001" <?> "Health check port"
    config :: w ::: Maybe FilePath <?> "The monocle configuration"
  }
  deriving stock (Generic)

instance ParseRecord (Macroscope Wrapped) where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

main :: IO ()
main = do
  args <- unwrapRecord "Macroscope lentille runner"
  config' <- fromMaybe (error "--config or CONFIG env is required") <$> lookupEnv "CONFIG"
  withClient (fromMaybe "http://web:8080" $ monocleUrl args) Nothing $ \client ->
    runMacroscope
      (port args)
      (fromMaybe config' $ config args)
      client
