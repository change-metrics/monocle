{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Macroscope.Main (runMacroscope)
import Monocle.Api.Client (withClient)
import Options.Generic
import Relude

data Macroscope w = Macroscope
  { monocleUrl :: w ::: Text <?> "The monocle API",
    config :: w ::: FilePath <?> "The monocle configuration",
    debug :: w ::: Bool <?> "Verbose mode"
  }
  deriving stock (Generic)

instance ParseRecord (Macroscope Wrapped) where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

main :: IO ()
main = do
  args <- unwrapRecord "Macroscope lentille runner"
  withClient (monocleUrl args) Nothing $ \client ->
    runMacroscope
      (debug args)
      (config args)
      client
