{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Copyright: (c) 2021 Monocle authors
-- SPDX-License-Identifier: AGPL-3.0-only
-- Maintainer: Monocle authors <fboucher@redhat.com>
--
-- Github system
module Lentille.Github (getGHSession) where

import Control.Monad.Catch (MonadThrow)
import Data.Aeson (FromJSON (..), Object, ToJSON (..), Value (Object), decode, encode, withObject, (.:))
import Data.Time.Clock (UTCTime)
import Lentille.Client (IsoTime (..), TaskData (..))
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types
import Relude

-------------------------------------------------------------------------------
-- Github system
-------------------------------------------------------------------------------

newtype MyRequest = MyRequest
  { query :: Text
  }
  deriving (Generic, Show, ToJSON)

newtype UrlFrag = UrlFrag
  { curl :: Text
  }
  deriving (Generic, Show)

instance FromJSON UrlFrag where
  parseJSON = withObject "UrlFrag" $ \v ->
    UrlFrag <$> v .: "url"

newtype SubjectFrag = SubjectFrag
  { subject :: UrlFrag
  }
  deriving (Generic, Show, FromJSON)

newtype TimelineItemNodesFrag = TimelineItemNodesFrag
  { tliNodes :: [SubjectFrag]
  }
  deriving (Generic, Show)

instance FromJSON TimelineItemNodesFrag where
  parseJSON = withObject "TimelineItemNodesFrag" $ \v ->
    TimelineItemNodesFrag <$> v .: "nodes"

newtype LabelFrag = LabelFrag {name :: Text} deriving (Generic, Show, FromJSON)

newtype LabelsItemNodesFrag = LabelsItemNodesFrag
  { liNodes :: [LabelFrag]
  }
  deriving (Generic, Show)

instance FromJSON LabelsItemNodesFrag where
  parseJSON = withObject "LabelsItemNodesFrag" $ \v ->
    LabelsItemNodesFrag <$> v .: "nodes"

data IssueItemFrag = IssueItemFrag
  { title :: Text,
    updatedAt :: UTCTime,
    id :: Text,
    url :: Text,
    labels :: LabelsItemNodesFrag,
    timelineItems :: TimelineItemNodesFrag
  }
  deriving (Generic, Show, FromJSON)

data IssueNodeFrag = IssueNodeFrag
  { mainNodes :: [IssueItemFrag],
    pageInfo :: PageInfo
  }
  deriving (Show)

instance FromJSON IssueNodeFrag where
  parseJSON = withObject "IssueNodeFrag" $ \v ->
    IssueNodeFrag <$> v .: "nodes" <*> v .: "pageInfo"

data PageInfo = PageInfo
  { endCursor :: Text,
    hasNextPage :: Bool
  }
  deriving (Generic, Show, FromJSON)

newtype SearchFrag = SearchFrag
  { search :: IssueNodeFrag
  }
  deriving (Generic, Show, FromJSON)

newtype DataFrag = DataFrag
  { maindata :: SearchFrag
  }
  deriving (Show)

instance FromJSON DataFrag where
  parseJSON = withObject "DataFrag" $ \v ->
    DataFrag <$> v .: "data"

type Organization = Text

type Repository = Text

type RepositoryFullname = Text

type UpdatedSinceDate = Text

type GHToken = ByteString

mkRepoName :: Organization -> Repository -> RepositoryFullname
mkRepoName org repo = org <> "/" <> repo

mkQuery :: RepositoryFullname -> UpdatedSinceDate -> Text
mkQuery rname datestr =
  unlines
    [ "{search(query:\"" <> gquery <> "\", type: ISSUE, first: 25) {",
      "pageInfo {hasNextPage endCursor}",
      "nodes {... on Issue {",
      "id",
      "updatedAt",
      "url",
      "title",
      "labels(first: 100) {nodes{name}}",
      "timelineItems(first: 100, itemTypes: [CONNECTED_EVENT]) {",
      "nodes {... on ConnectedEvent {subject {... on PullRequest {url} }}}}}}}}"
    ]
  where
    gquery = "repo:" <> rname <> " linked:pr " <> "updated:>=" <> datestr

getBaseRequest :: (MonadThrow m) => GHToken -> m Request
getBaseRequest token = do
  initReq <- parseRequest "https://api.github.com/graphql"
  pure
    initReq
      { method = "POST",
        requestHeaders =
          [ ("Authorization" :: HeaderName, "token " <> token),
            ("User-Agent" :: HeaderName, "change-metrics/lentille")
          ]
      }

data HasNextPage = Yes Text | No deriving (Show)

dataFragToTaskData :: DataFrag -> (HasNextPage, [TaskData])
dataFragToTaskData df =
  (getHasNextPage, concatMap toTaskData issues)
  where
    toTaskData :: IssueItemFrag -> [TaskData]
    toTaskData issue = map (mkTd issue) (getLinkedUrls issue)
    getLinkedUrls :: IssueItemFrag -> [Text]
    getLinkedUrls issue = map (curl . subject) (tliNodes $ timelineItems issue)
    mkTd :: IssueItemFrag -> Text -> TaskData
    mkTd issue curl' =
      TaskData
        (IsoTime $ updatedAt issue)
        curl'
        (getLabels issue)
        (Lentille.Github.id issue)
        (url issue)
        (title issue)
        "Low"
        "Low"
    issues = mainNodes (search (maindata df))
    getLabels :: IssueItemFrag -> [Text]
    getLabels issue = map name (liNodes $ labels issue)
    getHasNextPage :: HasNextPage
    getHasNextPage = if hasNextPage pinfo then Yes (endCursor pinfo) else No
    pinfo = pageInfo (search (maindata df))

getPage :: GHToken -> IO ()
getPage token = do
  manager <- newManager tlsManagerSettings
  baseReq <- getBaseRequest token
  let rFullname = mkRepoName "morucci" "reptest"
  let request =
        baseReq
          { requestBody =
              RequestBodyLBS $
                encode $
                  MyRequest
                    { query = mkQuery rFullname "2021-05-02"
                    }
          }
  r <- httpLbs request manager
  let decoded = decode (responseBody r) :: Maybe DataFrag
  case decoded of
    Just df -> displayTD hnp td
      where
        (hnp, td) = dataFragToTaskData df
    Nothing -> pure ()
  where
    displayTD :: HasNextPage -> [TaskData] -> IO ()
    displayTD hnp td = print (("HasNextPage: " :: [Char]) <> show hnp <> " TDs: " <> show td)

getGHSession :: IO ()
getGHSession = getPage ""
