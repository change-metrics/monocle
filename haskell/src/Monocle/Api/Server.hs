{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
module Monocle.Api.Server where

import qualified Data.Vector as V
import qualified Database.Bloodhound as BH
import Google.Protobuf.Timestamp as Timestamp
import Monocle.Search (ChangesQueryRequest, ChangesQueryResponse)
import qualified Monocle.Search as SearchPB
import Monocle.Search.Change (ELKChange (..))
import qualified Monocle.Search.Parser as P
import qualified Monocle.Search.Queries as Q
import qualified Monocle.Search.Query as Q
import Monocle.Search.Syntax (ParseError (..))
import Relude
import Servant (Handler)

searchChangeQuery :: BH.BHEnv -> ChangesQueryRequest -> Handler ChangesQueryResponse
searchChangeQuery bhEnv request = SearchPB.ChangesQueryResponse . Just <$> response
  where
    queryText = toStrict $ SearchPB.changesQueryRequestQuery request
    index = "monocle.changes.1." <> toStrict (SearchPB.changesQueryRequestIndex request)
    response = case P.parse queryText >>= Q.queryWithMods of
      Left (ParseError msg offset) ->
        pure
          . SearchPB.ChangesQueryResponseResultError
          . SearchPB.QueryError (toLazy (msg))
          $ (fromInteger . toInteger $ offset)
      Right query ->
        SearchPB.ChangesQueryResponseResultItems
          . SearchPB.Changes
          . V.fromList
          . map toResult
          <$> Q.changes bhEnv index query
    toResult :: ELKChange -> SearchPB.Change
    toResult change =
      let changeTitle = (toLazy $ elkchangeTitle change)
          changeUrl = (toLazy $ elkchangeUrl change)
          changeCreatedAt = (Just . Timestamp.fromUTCTime $ elkchangeCreatedAt change)
       in SearchPB.Change {..}
