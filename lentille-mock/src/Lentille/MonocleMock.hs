{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
module Lentille.MonocleMock
  ( withMockClient,
    withMockedManager,
    monocleMockApplication,
  )
where

import Data.Aeson (encode)
import qualified Data.HashMap.Strict as HM
import Data.List (lookup)
import Lentille.Client (MonocleClient, withClient)
import Network.HTTP.Mock (withMockedManager)
import Network.HTTP.Types.Status (status200)
import qualified Network.Wai as Wai
import Relude

withMockClient :: (MonocleClient -> IO ()) -> IO ()
withMockClient cb = withMockedManager monocleMockApplication go
  where
    go manager = withClient "http://localhost" (Just manager) cb

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
