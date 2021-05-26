{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Monocle queries
-- The goal of this module is to transform 'Query' into list of items
module Monocle.Search.Queries where

import Control.Monad.Catch (MonadThrow)
import Data.Aeson (FromJSON (..))
import qualified Database.Bloodhound as BH
import Monocle.Search.Change (Change (..))
import qualified Monocle.Search.Query as Q
import Monocle.Search.Syntax (SortOrder (..))
import qualified Network.HTTP.Client as HTTP
import Relude

mkEnv :: MonadIO m => Text -> m BH.BHEnv
mkEnv server = do
  manager <- liftIO $ HTTP.newManager HTTP.defaultManagerSettings
  pure $ BH.mkBHEnv (BH.Server server) manager

-- | Helper search func that can be replaced by a scanSearch
simpleSearch :: (FromJSON a, MonadThrow m, BH.MonadBH m) => BH.IndexName -> BH.Search -> m [BH.Hit a]
simpleSearch indexName search = do
  rawResp <- BH.searchByIndex indexName search
  resp <- BH.parseEsResponse rawResp
  case resp of
    Left e -> error (show e)
    Right x -> pure (BH.hits (BH.searchHits x))

runQuery :: MonadIO m => Text -> BH.BHEnv -> Text -> Q.Query -> m [Change]
runQuery documentType bhEnv index queryBase =
  liftIO $
    BH.runBH bhEnv $ do
      resp <- fmap BH.hitSource <$> simpleSearch (BH.IndexName index) search
      pure $ catMaybes resp
  where
    query =
      BH.QueryBoolQuery $
        BH.mkBoolQuery [BH.TermQuery (BH.Term "type" documentType) Nothing, (Q.queryBH queryBase)] [] [] []
    search =
      (BH.mkSearch (Just query) Nothing)
        { BH.size = BH.Size (Q.queryLimit queryBase),
          BH.sortBody = toSortBody <$> Q.queryOrder queryBase
        }
    toSortBody (field, order) =
      [ BH.DefaultSortSpec
          ( BH.DefaultSort (BH.FieldName field) (sortOrder order) Nothing Nothing Nothing Nothing
          )
      ]
    sortOrder order = case order of
      Asc -> BH.Ascending
      Desc -> BH.Descending

changes :: MonadIO m => BH.BHEnv -> Text -> Q.Query -> m [Change]
changes = runQuery "Change"
