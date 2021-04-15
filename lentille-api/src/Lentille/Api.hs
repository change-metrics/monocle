{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lentille.Api (app) where

import Data.Time (UTCTime, getCurrentTime)
import Network.Wai (Application)
import Relude
import Servant (Handler, Server, serve)
import Servant.API

type ReqIndex = QueryParam "index" Text

type TrackerAPI =
  "tracker_data" :> ReqIndex :> Get '[JSON] UTCTime
    :<|> "tracker_data" :> "commit" :> ReqIndex :> QueryParam "date" UTCTime :> Post '[PlainText] Text

type ChangeAPI = "change_data" :> Get '[JSON] Text

type MonocleAPI = "api" :> "1" :> (TrackerAPI :<|> ChangeAPI)

server :: IORef UTCTime -> Server MonocleAPI
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

app :: MonadIO m => m Application
app = do
  now <- liftIO getCurrentTime
  db <- newIORef now
  putTextLn "Serving api"
  pure $ serve api (server db)
  where
    api :: Proxy MonocleAPI
    api = Proxy
