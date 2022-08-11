--
module Lentille.Bugzilla.Mock (bugzillaMockClient, bugzillaMockApplication) where

import Control.Concurrent (forkIO)
import Control.Concurrent.QSem qualified as QSem
import Data.ByteString qualified as BS
import Network.HTTP.Types.Status (status200)
import Network.Socket (Socket)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Relude
import Web.RedHatBugzilla qualified as BZ
import Witch (from)

bugzillaMockApplication :: Wai.Application
bugzillaMockApplication req respond = do
  -- putTextLn $ "Got bugzilla req: " <> show req
  putTextLn $ "Serving : " <> show (Wai.rawPathInfo req <> Wai.rawQueryString req)
  respData <- case Wai.rawPathInfo req of
    "/rest/bug/1791815" -> BS.readFile (base <> "rhbz1791815.json")
    "/rest/bug" -> BS.readFile (base <> "rhbzsearch.json")
    x -> error $ "Unknown path: " <> show x
  respond $ Wai.responseLBS status200 mempty (from respData)
 where
  base = "./test/data/"

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
bugzillaMockClient = BZ.anonymousSession <$> bugzillaMockServer
