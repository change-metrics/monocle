{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- |
module Lentille.ELK where

import Control.Monad.Catch (MonadThrow)
import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    Value (..),
    defaultOptions,
    genericParseJSON,
    genericToJSON,
    object,
    (.:),
    (.=),
  )
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Database.Bloodhound as BH
import Network.HTTP.Client (defaultManagerSettings, responseBody, responseStatus)
import Network.HTTP.Types.Status (Status (..))
import Relude

data TaskDataMapping = TaskDataMapping deriving stock (Eq, Show)

instance ToJSON TaskDataMapping where
  toJSON TaskDataMapping =
    object
      [ "properties"
          .= object ["keywords" .= object ["type" .= ("keyword" :: Text)]]
      ]

data TaskData = TaskData
  { tdId :: Int,
    url :: Text,
    keywords :: [Text]
  }
  deriving stock (Eq, Generic, Show)

instance ToJSON TaskData where
  toJSON = genericToJSON defaultOptions

instance FromJSON TaskData where
  parseJSON = genericParseJSON defaultOptions

getIndices :: (MonadThrow m, BH.MonadBH m) => m [Text]
getIndices = do
  indices <- BH.listIndices
  pure . filter (T.isPrefixOf "monocle.") . map (\(BH.IndexName name) -> name) $ indices

data ChangeUrl = ChangeUrl
  { changeUrl :: Text
  }
  deriving stock (Eq, Show)

instance FromJSON ChangeUrl where
  parseJSON (Object v) = ChangeUrl <$> v .: "url"
  parseJSON _ = empty

-- | A function to replace '//' by '/' in change urls
patchUrlsDoubleSlash :: (MonadThrow m, BH.MonadBH m) => BH.IndexName -> m ()
patchUrlsDoubleSlash indexName = do
  initialSearchResult <- BH.getInitialSortedScroll indexName getUrlsSearch
  let (hits', josh) = case initialSearchResult of
        Right BH.SearchResult {..} -> (BH.hits searchHits, scrollId)
        Left err -> error $ "get init failed: " <> show err
  go (0 :: Int) hits' josh
  where
    go :: (MonadThrow m, BH.MonadBH m) => Int -> [BH.Hit ChangeUrl] -> Maybe BH.ScrollId -> m ()
    go 100 _ _ = putTextLn "Stopping after 100 batch"
    go _ _ Nothing = error "No scrollId"
    go _ [] _ = putTextLn "no more match"
    go idx hits (Just scrollId') = do
      let urls = map getUrls hits
          count = length urls
      putTextLn $ "Patching " <> show count <> " changes, e.g.: " <> show (head <$> nonEmpty urls)

      -- Patch urls
      let updateOps = V.fromList $ map mkUpdateOp urls
      resp <- BH.bulk updateOps
      _ <- BH.refreshIndex indexName

      unless (respIsOk resp) (error $ "Bulk update failed: " <> show resp)

      -- Get more hits
      resp' <- BH.advanceScroll scrollId' 60
      case resp' of
        Right BH.SearchResult {..} -> (go (idx + 1) (BH.hits searchHits) scrollId)
        Left err -> error $ "advance scroll failed: " <> show err

    respIsOk resp = case responseStatus resp of
      Status code _ | code >= 200 && code <= 300 -> True
      _ -> False

    patchUrl :: Text -> Text
    patchUrl url' =
      let (base, num) = T.breakOnEnd "//" url'
       in T.dropEnd 2 base <> "/" <> num

    mkUpdateOp :: (BH.DocId, Text) -> BH.BulkOperation
    mkUpdateOp (docId, url') =
      let patch = (object ["url" .= String (patchUrl url')])
       in BH.BulkUpdate indexName docId patch

    getUrls :: BH.Hit ChangeUrl -> (BH.DocId, Text)
    getUrls hit = case hit of
      BH.Hit _index docId _score (Just (ChangeUrl url')) _sort _fields _highlight -> (docId, url')
      _ -> error "No url found"
    getUrlsQuery =
      BH.QueryRegexpQuery $
        BH.RegexpQuery (BH.FieldName "url") (BH.Regexp "https://.*//.*") (BH.NoRegexpFlags) Nothing
    getUrlsSource = BH.SourceIncludeExclude (BH.Include [BH.Pattern "url"]) (BH.Exclude [])
    getUrlsSearch = (BH.mkSearch (Just getUrlsQuery) Nothing) {BH.source = (Just getUrlsSource), BH.size = BH.Size 5000}

demo :: Text -> Text -> IO ()
demo server index = BH.withBH defaultManagerSettings (BH.Server server) go
  where
    indexSettings = BH.IndexSettings (BH.ShardCount 1) (BH.ReplicaCount 0)
    testIndex = BH.IndexName index
    go = do
      monocleIndices <- getIndices
      print monocleIndices

      -- set up index
      _ <- BH.createIndex indexSettings testIndex
      True <- BH.indexExists testIndex
      _ <- BH.putMapping testIndex TaskDataMapping

      -- add data
      _resp <- traverse postDoc (fakeTaskDatas 50)

      -- search
      let _queryTerm = BH.TermQuery (BH.Term "url" "bugzilla") Nothing
          queryMatch = BH.QueryMatchQuery (BH.mkMatchQuery (BH.FieldName "keywords") (BH.QueryString "FeatureRequest"))
          search = BH.mkSearch (Just queryMatch) Nothing
      _searchResp <- BH.searchByIndex testIndex search
      -- print searchResp

      -- aggregate
      let terms = BH.TermsAgg $ (BH.mkTermsAggregation "") {BH.term = Left "keywords"}
          aggSearch = BH.mkAggregateSearch Nothing (BH.mkAggregations "demo" terms)
      aggResp <- BH.searchByIndex testIndex aggSearch
      putTextLn (decodeUtf8 $ responseBody aggResp)

    postDoc td = BH.indexDocument testIndex BH.defaultIndexDocumentSettings td (BH.DocId . show . tdId $ td)

    fakeTaskDatas :: Int -> [TaskData]
    fakeTaskDatas x = map mkTaskData [0 .. x]
      where
        mkTaskData idx = TaskData idx ("https://bugzilla/show_bug.cgi?id=" <> show idx) (mkKeywords idx)
        mkKeywords :: Int -> [Text]
        mkKeywords idx
          | idx `mod` 5 == 0 = ["FeatureRequest", "Patch"]
          | idx `mod` 3 == 0 = ["Triaged"]
          | otherwise = ["Bug"]
