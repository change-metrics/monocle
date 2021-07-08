-- |
module Monocle.Mock
  ( withMockClient,
  )
where

import qualified Data.HashMap.Strict as HM
import Network.HTTP.Client (Manager)
import Network.HTTP.Mock (withMockedManager)
import Network.HTTP.Types.Status (status200)
import qualified Network.Wai as Wai
import Relude

type WithClient client = Text -> Maybe Manager -> (client -> IO ()) -> IO ()

-- | Create a client using a fake http manager that serve a mock application
withMockClient :: WithClient client -> (client -> IO ()) -> IO ()
withMockClient withClient cb = withMockedManager monocleMockApplication go
  where
    go manager = withClient "http://localhost" (Just manager) cb

-- | The mocked responses dictionary (VERB, path) -> reply
monocleMockResponse :: HM.HashMap (ByteString, ByteString) LByteString
monocleMockResponse =
  fromList
    [ (("POST", "/api/1/task_data_get_last_updated"), "{\"timestamp\": \"2021-01-01T00:00:00Z\"}")
    ]

-- | The application to serve mocked responses.
monocleMockApplication :: Wai.Application
monocleMockApplication req respond =
  respond $ Wai.responseLBS status200 headers response
  where
    headers = [("Content-Type", "application/json")]
    response = fromMaybe (error ("unknown path: " <> show requestPath)) responseM
    requestPath = Wai.rawPathInfo req
    responseM :: Maybe LByteString
    responseM = HM.lookup (Wai.requestMethod req, requestPath) monocleMockResponse
