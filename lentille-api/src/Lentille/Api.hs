{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lentille.Api (app) where

#ifdef USE_OPENAPI
import Data.OpenApi hiding (server, Server) -- (OpenApi, ((.~)))
import Servant.OpenApi (toOpenApi)
import Control.Lens.Operators
#endif
import Data.Time (UTCTime, getCurrentTime)
import Network.Wai (Application)
import Relude
import Servant (Handler, Server, serve)
import Servant.API

type ReqIndex = QueryParam "index" Text

type TrackerApi =
  "tracker_data" :> ReqIndex :> Get '[JSON] UTCTime
    :<|> "tracker_data" :> "commit" :> ReqIndex :> QueryParam "date" UTCTime :> Post '[PlainText] Text

type ChangeApi = "change_data" :> Get '[JSON] Text

type MonocleApi = "api" :> "1" :> (TrackerApi :<|> ChangeApi)

monocleApi :: Proxy MonocleApi
monocleApi = Proxy

type Db = IORef UTCTime

server :: Db -> Server MonocleApi
server db = tdApp :<|> changeApp
  where
    changeApp = changeGet

    changeGet :: Handler Text
    changeGet = pure "a change"

    tdApp = tdGet :<|> tdPost

    tdGet :: Maybe Text -> Handler UTCTime
    tdGet (Just _idx) = readIORef db
    tdGet _ = error "Missing index"

    tdPost :: Maybe Text -> Maybe UTCTime -> Handler Text
    tdPost (Just _idx) (Just ts) = do
      writeIORef db ts
      pure "Commited"
    tdPost _ _ = error "Missing arguments"

#ifdef USE_OPENAPI
type SwaggerAPI = "swagger.json" :> Get '[JSON] OpenApi

type MonocleWithOpenApi = SwaggerAPI :<|> MonocleApi

monocleSwagger :: OpenApi
monocleSwagger =
  toOpenApi monocleApi
  & info.title .~ "Monocle API"
  & info.version .~ "2"
  & info.description ?~ "Generated swagger API"

finalApi :: Proxy MonocleWithOpenApi
finalApi = Proxy

finalServer :: Db -> Server MonocleWithOpenApi
finalServer db = return monocleSwagger :<|> server db
#else
finalApi :: Proxy MonocleApi
finalApi = monocleApi

finalServer :: Db -> Server MonocleApi
finalServer = server
#endif

app :: MonadIO m => m Application
app = do
  now <- liftIO getCurrentTime
  db <- newIORef now
  putTextLn "Serving api"
  pure $ serve finalApi (finalServer db)
