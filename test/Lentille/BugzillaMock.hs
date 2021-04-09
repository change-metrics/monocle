{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
module Lentille.BugzillaMock (bugzillaMockClient, bugzillaMockApplication) where

import Control.Concurrent (forkIO)
import qualified Control.Concurrent.QSem as QSem
import qualified Data.ByteString as BS
import Network.HTTP.Types.Status (status200)
import Network.Socket (Socket)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Relude
import qualified Web.Bugzilla.RedHat as BZ

bugzillaMockApplication :: Wai.Application
bugzillaMockApplication req respond = do
  -- putTextLn $ "Got bugzilla req: " <> show req
  putTextLn $ "Serving : " <> show (Wai.rawPathInfo req <> Wai.rawQueryString req)
  respData <- case Wai.rawPathInfo req of
    "/rest/bug/1791815" -> BS.readFile "./test/data/rhbz1791815.json"
    "/rest/bug" -> BS.readFile "./test/data/rhbzsearch.json"
    x -> error $ "Unknown path: " <> show x
  respond $ Wai.responseLBS status200 mempty (toLazy respData)

-- | Lowlevel wai application server
bugzillaMockServerThread :: QSem.QSem -> Warp.Port -> Socket -> IO ()
bugzillaMockServerThread sem port skt = do
  Warp.runSettingsSocket settings skt bugzillaMockApplication
  where
    settings =
      Warp.setPort port $
        Warp.setBeforeMainLoop (QSem.signalQSem sem) Warp.defaultSettings

-- | Create a WARP server and return its URL
bugzillaMockServer :: IO Text
bugzillaMockServer = do
  sem <- QSem.newQSem 0
  (port, skt) <- Warp.openFreePort
  let url = "http://localhost:" <> show port
  _ <- forkIO (bugzillaMockServerThread sem port skt)
  QSem.waitQSem sem
  pure url

bugzillaMockClient :: IO BZ.BugzillaSession
bugzillaMockClient = do
  url <- bugzillaMockServer
  -- putTextLn $ "Creating bz client with " <> show url
  ctx <- BZ.newBugzillaContext url
  pure $ BZ.anonymousSession ctx
