{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Monocle.WebServerApi where

import Data.Aeson
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Servant

type MonocleAPI =
  "indices" :> Get '[JSON] [Indice]
    :<|> "infos" :> Get '[JSON] Info

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

info :: Info
info = Info "1.0.0"

indices :: [Indice]
indices =
  [ Indice "OpenStack" "The OpenStack index",
    Indice "OpenShift" "The OpenShift index"
  ]

server :: Server MonocleAPI
server =
  return indices
    :<|> return info

monocleAPI :: Proxy MonocleAPI
monocleAPI = Proxy

app :: Application
app = serve monocleAPI server

main :: IO ()
main = run 8081 app
