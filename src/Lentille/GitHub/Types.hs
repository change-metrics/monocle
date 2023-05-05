{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

-- | The API global types
module Lentille.GitHub.Types where

import Data.Morpheus.Client (declareGlobalTypesByName)
import Lentille.GraphQL (ghSchemaLocation)
import Monocle.Prelude
import Monocle.Protob.Change (Change, ChangeEvent)

newtype DateTime = DateTime Text deriving (Show, Eq, FromJSON)

newtype URI = URI {unURI :: Text} deriving (Show, Eq, FromJSON)

newtype GitObjectID = GitObjectID Text deriving (Show, Eq, FromJSON)

declareGlobalTypesByName
  ghSchemaLocation
  [ "MergeableState"
  , "PullRequestReviewDecision"
  , "PullRequestState"
  , "PullRequestReviewState"
  ]

type Changes = (Change, [ChangeEvent])
