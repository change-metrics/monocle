{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
module Lentille.MonocleMock (monocleMockApplication) where

import Data.Aeson (encode)
import qualified Data.HashMap.Strict as HM
import Data.List (lookup)
import Network.HTTP.Types.Status (status200)
import qualified Network.Wai as Wai
import Relude

fakeIndices :: [Text]
fakeIndices = ["indice1", "indice2"]

monocleMockResponse :: HM.HashMap ByteString LByteString
monocleMockResponse =
  fromList
    [ ("/api/0/indices", encode fakeIndices),
      ("/api/0/task_tracker/updated_since_date", "\"2021-01-01T00:00:00Z\""),
      ("/api/0/amend/tracker_data", "[]")
    ]

monocleMockApplication :: Wai.Application
monocleMockApplication req respond =
  respond $ Wai.responseLBS status200 headers response
  where
    headers = [("Content-Type", "application/json")]
    response =
      case lookup "apikey" (Wai.queryString req) of
        Just (Just "failme") -> "[42]"
        _ -> fromMaybe (error ("unknown path: " <> show requestPath)) responseM
    requestPath = Wai.rawPathInfo req
    responseM :: Maybe LByteString
    responseM = HM.lookup requestPath monocleMockResponse
