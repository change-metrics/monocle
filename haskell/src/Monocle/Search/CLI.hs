{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
module Monocle.Search.CLI (searchMain, runQuery) where

import Control.Monad.Catch (MonadThrow)
import Data.Aeson (FromJSON (..))
import qualified Data.Aeson as Aeson
import qualified Database.Bloodhound as BH
import Monocle.Search.Change (Change (..))
import qualified Monocle.Search.Parser as P
import qualified Monocle.Search.Query as Q
import Relude

parseQuery :: Text -> Text
parseQuery code = either id decodeUtf8 query
  where
    query = do
      expr <- P.parse code
      Aeson.encode <$> Q.query expr

-- | Helper search func that can be replaced by a scanSearch
simpleSearch :: (FromJSON a, MonadThrow m, BH.MonadBH m) => BH.IndexName -> BH.Search -> m [BH.Hit a]
simpleSearch indexName search = do
  rawResp <- BH.searchByIndex indexName search
  resp <- BH.parseEsResponse rawResp
  case resp of
    Left e -> error (show e)
    Right x -> pure (BH.hits (BH.searchHits x))

runQuery :: MonadIO m => BH.BHEnv -> Text -> BH.Query -> m [Change]
runQuery bhEnv index queryBase =
  liftIO $
    BH.runBH bhEnv $ do
      resp <- fmap BH.hitSource <$> simpleSearch (BH.IndexName index) search
      pure $ catMaybes resp
  where
    query = BH.QueryBoolQuery $ BH.mkBoolQuery [BH.TermQuery (BH.Term "type" "Change") Nothing, queryBase] [] [] []
    search = (BH.mkSearch (Just query) Nothing) {BH.size = BH.Size 1000}

searchMain :: MonadIO m => m ()
searchMain = do
  args <- map toText <$> getArgs
  case args of
    ["--parse", query] -> putTextLn $ parseQuery query
    [elkUrl, index, code] -> do
      let query = case P.parse code >>= Q.query of
            Left err -> error $ "Invalid query: " <> err
            Right q -> q
      bhEnv <- Q.mkEnv elkUrl
      changes <- runQuery bhEnv index query
      mapM_ (putTextLn . show) (take 2 changes)
      putTextLn $ "Got : " <> show (length changes) <> " results"
    _ -> putTextLn "usage: elk-url index query"
