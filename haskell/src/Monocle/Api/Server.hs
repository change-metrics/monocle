{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
module Monocle.Api.Server where

import qualified Data.Vector as V
import qualified Database.Bloodhound as BH
import Google.Protobuf.Timestamp as Timestamp
import Monocle.Search (ChangesQueryRequest, ChangesQueryResponse, FieldsRequest, FieldsResponse (..))
import qualified Monocle.Search as SearchPB
import Monocle.Search.Change (ELKChange (..))
import qualified Monocle.Search.Parser as P
import qualified Monocle.Search.Queries as Q
import qualified Monocle.Search.Query as Q
import Monocle.Search.Syntax (ParseError (..))
import Proto3.Suite (Enumerated (..))
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
          changeRepositoryFullname = (toLazy $ elkchangeRepositoryFullname change)
          changeState = (toLazy $ elkchangeState change)
          changeBranch = (toLazy $ elkchangeBranch change)
       in SearchPB.Change {..}

searchFields :: FieldsRequest -> Handler FieldsResponse
searchFields = const $ pure response
  where
    response :: FieldsResponse
    response = FieldsResponse . V.fromList . map toResult $ Q.fields
    toResult (name, (fieldType', _realname, desc)) =
      let fieldName = toLazy name
          fieldDescription = toLazy desc
          fieldType = Enumerated . Right $ fieldType'
       in SearchPB.Field {..}
