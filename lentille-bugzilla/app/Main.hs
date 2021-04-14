{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Lentille.Bugzilla
import Options.Generic
import Relude
import qualified Streaming.Prelude as S

data BZCli w = BZCli
  { bugzillaUrl :: w ::: Maybe Text <?> "The bugzilla url",
    since :: w ::: String <?> "Get bugs since"
  }
  deriving stock (Generic)

instance ParseRecord (BZCli Wrapped) where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

deriving stock instance Show (BZCli Unwrapped)

main :: IO ()
main = do
  args <- unwrapRecord "Lentille BZ worker"
  let sinceTS = fromMaybe (error "Couldn't parse since") (readMaybe (since args))
      bzUrl = fromMaybe "bugzilla.redhat.com" (bugzillaUrl args)
  bzSession <- getBugzillaSession bzUrl
  S.mapM_ print (getBZData bzSession sinceTS)
