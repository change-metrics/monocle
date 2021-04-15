{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
module Lentille.MonocleMock
  ( withMockClient,
  )
where

import Data.Aeson (encode)
import qualified Data.HashMap.Strict as HM
import Data.List (lookup)
import Network.HTTP.Client (Manager)
import Network.HTTP.Mock (withMockedManager)
import Network.HTTP.Types.Status (status200)
import qualified Network.Wai as Wai
import Relude

type WithClient client = Text -> Maybe Manager -> (client -> IO ()) -> IO ()

withMockClient :: WithClient client -> (client -> IO ()) -> IO ()
withMockClient withClient cb = withMockedManager monocleMockApplication go
  where
    go manager = withClient "http://localhost" (Just manager) cb

fakeIndices :: [Text]
fakeIndices = ["indice1", "indice2"]

monocleMockResponse :: HM.HashMap (ByteString, ByteString) LByteString
monocleMockResponse =
  fromList
    [ (("GET", "/api/0/indices"), encode fakeIndices),
      (("GET", "/api/0/tracker_data"), "\"2021-01-01T00:00:00Z\""),
      (("POST", "/api/0/tracker_data"), "[]"),
      (("POST", "/api/0/tracker_data/commit"), "Commited")
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
    responseM = HM.lookup (Wai.requestMethod req, requestPath) monocleMockResponse
