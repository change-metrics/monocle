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
    object,
    (.:),
    (.=),
  )
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Database.Bloodhound as BH
import Lentille (TaskData (..))
import Network.HTTP.Client (Response, defaultManagerSettings, responseBody, responseStatus)
import Network.HTTP.Types.Status (Status (..))
import Relude

data TaskDataMapping = TaskDataMapping deriving stock (Eq, Show)

instance ToJSON TaskDataMapping where
  toJSON TaskDataMapping =
    object
      [ "properties"
          .= object ["keywords" .= object ["type" .= ("keyword" :: Text)]]
      ]

getIndices :: (MonadThrow m, BH.MonadBH m) => m [Text]
getIndices = do
  indices <- BH.listIndices
  pure . filter (T.isPrefixOf "monocle.") . map (\(BH.IndexName name) -> name) $ indices

-- A document record containing tasks data
data ChangeTD = ChangeTD
  { changeTDUrl :: Text,
    changeTDData :: [TaskData]
  }
  deriving stock (Eq, Show)

instance FromJSON ChangeTD where
  parseJSON (Object v) = do
    url <- v .: "url"
    -- This decode tasks_data as either a single item, or a list of items
    tds <- v .: "tasks_data"
    case tds of
      td@(Object _) -> do
        td' <- parseJSON td
        pure $ ChangeTD url [td']
      tds'@(Array _) -> ChangeTD url <$> parseJSON tds'
      _ -> empty
  parseJSON _ = empty

-- A simpler document record without task data
data ChangeNoTD = ChangeNoTD
  { changeNoTDUrl :: Text,
    changeNoTDType :: Text
  }
  deriving stock (Eq, Show)

instance FromJSON ChangeNoTD where
  parseJSON (Object v) = ChangeNoTD <$> v .: "url" <*> v .: "type"
  parseJSON _ = empty

-- | Helper func to check if a response is ok
isRespOk :: Response a -> Bool
isRespOk resp = case responseStatus resp of
  Status code _ | code >= 200 && code <= 300 -> True
  _ -> False

-- | Helper search func that can be replaced by a scanSearch
simpleSearch :: (FromJSON a, MonadThrow m, BH.MonadBH m) => BH.IndexName -> BH.Search -> m [BH.Hit a]
simpleSearch indexName search = do
  rawResp <- BH.searchByIndex indexName search
  resp <- BH.parseEsResponse rawResp
  case resp of
    Left e -> error (show e)
    Right x -> pure (BH.hits (BH.searchHits x))

-- | A function to add task data to other events
patchCopyTaskData :: (MonadThrow m, BH.MonadBH m) => BH.IndexName -> m ()
patchCopyTaskData indexName = do
  tds <- getTaskDataFromChanges
  putTextLn $ "Copying tasks data from " <> show (length tds) <> " changes"
  traverse_ copyTaskDataToOtherEtype (map getTds tds)
  putTextLn "Done."
  where
    getTds :: BH.Hit ChangeTD -> ChangeTD
    getTds hit = fromMaybe (error "No source") (BH.hitSource hit)

    -- Return documents with tasks_data (e.g. etype == "Change")
    getTaskDataFromChanges :: (MonadThrow m, BH.MonadBH m) => m [BH.Hit ChangeTD]
    getTaskDataFromChanges =
      -- simpleSearch indexName getTDsSearch
      BH.scanSearch indexName search
      where
        search =
          let -- We look for existing tasks_data
              query = fieldExists "tasks_data"
              -- And we only want document of type `Change`
              filter' = BH.TermQuery (BH.Term "type" "Change") Nothing
           in (BH.mkSearch (Just query) (Just $ BH.Filter filter'))
                { BH.source = sourceFields ["url", "tasks_data"]
                }

    -- Copy the task data
    copyTaskDataToOtherEtype :: (MonadThrow m, BH.MonadBH m) => ChangeTD -> m ()
    copyTaskDataToOtherEtype ctd = do
      notds <- getChangesWithoutTD (changeTDUrl ctd)
      putTextLn $
        "Going to copy task datas from "
          <> changeTDUrl ctd
          <> " to "
          <> show (length notds)
          <> " other document"
      doBulkCopy (changeTDData ctd) (map BH.hitDocId notds)

    -- Return documents matching a change url without tasks_data (e.g. etype /= "Change")
    getChangesWithoutTD :: (MonadThrow m, BH.MonadBH m) => Text -> m [BH.Hit ChangeNoTD]
    getChangesWithoutTD url =
      -- simpleSearch indexName (getNoTDsSearch url)
      BH.scanSearch indexName search
      where
        search =
          let -- We look for document matching the given `url`
              query = urlMatch url
              -- And we only want document without a `tasks_data` field
              filter' = BH.QueryBoolQuery $ BH.mkBoolQuery [] [] [fieldExists "tasks_data"] []
           in (BH.mkSearch (Just query) (Just $ BH.Filter filter'))
                { BH.source = sourceFields ["url", "type"]
                }

    -- Do the actual copy
    doBulkCopy :: (MonadThrow m, BH.MonadBH m) => [TaskData] -> [BH.DocId] -> m ()
    doBulkCopy _ [] = pure ()
    doBulkCopy td docs = do
      let updateOps = V.fromList $ map mkUpdateOp docs
      resp <- BH.bulk updateOps
      _ <- BH.refreshIndex indexName
      unless (isRespOk resp) (error $ "Bulk update failed: " <> show resp)
      where
        mkUpdateOp doc =
          let patch = (object ["tasks_data" .= toJSON td])
           in BH.BulkUpdate indexName doc patch

    fieldExists x = BH.QueryExistsQuery $ BH.FieldName x
    urlMatch url = BH.TermQuery (BH.Term "url" url) Nothing
    sourceFields xs = Just $ BH.SourceIncludeExclude (BH.Include $ map BH.Pattern xs) (BH.Exclude [])

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

      unless (isRespOk resp) (error $ "Bulk update failed: " <> show resp)

      -- Get more hits
      resp' <- BH.advanceScroll scrollId' 60
      case resp' of
        Right BH.SearchResult {..} -> (go (idx + 1) (BH.hits searchHits) scrollId)
        Left err -> error $ "advance scroll failed: " <> show err

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

    postDoc td = BH.indexDocument testIndex BH.defaultIndexDocumentSettings td (BH.DocId . tdTid $ td)

    fakeTaskDatas :: Int -> [TaskData]
    fakeTaskDatas _x = []

{-
map mkTaskData [0 .. x]
where
  mkTaskData idx = TaskData idx ("https://bugzilla/show_bug.cgi?id=" <> show idx) (mkKeywords idx)
  mkKeywords :: Int -> [Text]
  mkKeywords idx
    | idx `mod` 5 == 0 = ["FeatureRequest", "Patch"]
    | idx `mod` 3 == 0 = ["Triaged"]
    | otherwise = ["Bug"]
-}
