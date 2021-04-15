{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Lentille OpenApi generator
module Lentille.OpenApi (monocleSwagger, api, server) where

import Control.Lens.Operators
import Data.OpenApi hiding (Server, server)
import qualified Lentille.Api as Lentille
import Relude
import Servant (Server)
import Servant.API
import Servant.OpenApi (toOpenApi)

type SwaggerAPI = "swagger.json" :> Get '[JSON] OpenApi

type MonocleWithOpenApi = SwaggerAPI :<|> Lentille.MonocleApi

monocleSwagger :: OpenApi
monocleSwagger =
  toOpenApi Lentille.api
    & info . title .~ "Monocle API"
    & info . version .~ "2"
    & info . description ?~ "Generated swagger API"

api :: Proxy MonocleWithOpenApi
api = Proxy

server :: Lentille.Db -> Server MonocleWithOpenApi
server db = return monocleSwagger :<|> Lentille.server db
