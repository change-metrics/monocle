{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Monocle.WebServerApi where

import Data.Aeson
import GHC.Generics
import qualified Monocle.Api.Config as Config
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Relude
import Servant

type MonocleAPI =
  "indices" :> Get '[JSON] [Text]
    :<|> "infos" :> Get '[JSON] Info
    :<|> "query" :> ReqBody '[JSON] SimpleQueryInput :> Post '[JSON] SimpleQueryResponse

data Indice = Indice
  { name :: String,
    description :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Indice

newtype Info = Info
  { version :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Info

data SimpleQueryInput = SimpleQueryInput
  { indice :: String,
    qs :: String
  }
  deriving (Eq, Show, Generic)

instance FromJSON SimpleQueryInput

newtype SimpleQueryResponse = SimpleQueryResponse
  { response :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON SimpleQueryResponse

info :: Info
info = Info "1.0.0"

server :: [Config.Tenant] -> Server MonocleAPI
server tenants =
  return (map Config.unTenant tenants)
    :<|> return info
    :<|> simpleQuery
  where
    -- curl -vi -XPOST -H 'Accept: application/json' -H 'Content-type: application/json' -d '{"indice":"test", "qs":"my super query"}' http://localhost:8081/query
    simpleQuery :: SimpleQueryInput -> Handler SimpleQueryResponse
    simpleQuery inputData = return $ SimpleQueryResponse $ "Input query is: " <> qs inputData

monocleAPI :: Proxy MonocleAPI
monocleAPI = Proxy

app :: [Config.Tenant] -> Application
app tenants = serve monocleAPI (server tenants)

main :: MonadIO m => Int -> FilePath -> m ()
main port configFile = do
  tenants <- Config.loadConfig configFile
  liftIO $ run port (app tenants)
