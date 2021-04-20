{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- |
module Lentille.ELK where

import Control.Monad.Catch (MonadThrow)
import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    defaultOptions,
    genericParseJSON,
    genericToJSON,
    object,
    (.=),
  )
import qualified Data.Text as T
import qualified Database.Bloodhound as BH
import Network.HTTP.Client (defaultManagerSettings, responseBody)
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
