{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | The Monocle API HTTP type and servant
module Monocle.Api.HTTP (MonocleAPI, server) where

import Monocle.Api.Env
import Monocle.Api.PBJSON (PBJSON)
import Monocle.Api.Server (searchChangeQuery, searchFields)
import Monocle.Search (ChangesQueryRequest, ChangesQueryResponse, FieldsRequest, FieldsResponse)
import Servant

type MonocleAPI =
  "api" :> "2" :> "search" :> "changes" :> ReqBody '[JSON] ChangesQueryRequest :> Post '[PBJSON, JSON] ChangesQueryResponse
    :<|> "api" :> "2" :> "search_fields" :> ReqBody '[JSON] FieldsRequest :> Post '[PBJSON, JSON] FieldsResponse

server :: ServerT MonocleAPI AppM
server =
  searchChangeQuery
    :<|> searchFields
