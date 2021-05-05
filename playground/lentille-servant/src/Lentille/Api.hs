{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lentille.Api (MonocleApi, Db, api, server) where

import Data.Time (UTCTime)
import Relude
import Servant (Handler, Server)
import Servant.API

type ReqIndex = QueryParam "index" Text

-- | The task api defined as a servant type
type TaskApi =
  "task_data" :> ReqIndex :> Get '[JSON] UTCTime
    :<|> "task_data" :> Capture "index" Text :> Get '[JSON] [UTCTime]
    :<|> "task_data" :> "commit" :> ReqIndex :> QueryParam "date" UTCTime :> Post '[JSON] Text

-- | Another api type
type ChangeApi = "change_data" :> Get '[JSON] Text

-- | Types can be composed to create the final api
type MonocleApi = "api" :> "1" :> (TaskApi :<|> ChangeApi)

-- | A dummy database that only store a timestamp
type Db = IORef UTCTime

-- | The server implementation typecheck the api type
server :: Db -> Server MonocleApi
server db = taskApp :<|> changeApp
  where
    changeApp = changeGet

    changeGet :: Handler Text
    changeGet = pure "a change"

    taskApp = taskGet :<|> taskGet' :<|> taskPost

    taskGet' :: Text -> Handler [UTCTime]
    taskGet' _idx = do
      d <- readIORef db
      pure [d, d]

    taskGet :: Maybe Text -> Handler UTCTime
    taskGet (Just _idx) = readIORef db
    taskGet _ = error "Missing index"

    taskPost :: Maybe Text -> Maybe UTCTime -> Handler Text
    taskPost (Just _idx) (Just ts) = do
      writeIORef db ts
      pure "Commited"
    taskPost _ _ = error "Missing arguments"

api :: Proxy MonocleApi
api = Proxy
