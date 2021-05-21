{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
module Monocle.Search.CLI (searchMain) where

import qualified Data.Aeson as Aeson
import qualified Database.Bloodhound as BH
import qualified Monocle.Search.Parser as P
import qualified Monocle.Search.Query as Q
import Network.HTTP.Client (Response, defaultManagerSettings, responseBody, responseStatus)
import Relude

parseQuery :: Text -> Text
parseQuery code = either id decodeUtf8 query
  where
    query = do
      expr <- P.parse code
      Aeson.encode <$> Q.query expr

runQuery :: MonadIO m => Text -> Text -> Text -> m ()
runQuery server index code =
  liftIO $
    BH.withBH defaultManagerSettings (BH.Server server) $ do
      resp <- BH.searchByIndex (BH.IndexName index) search
      print resp
  where
    query = fromRight (error "Invalid query") (P.parse code >>= Q.query)
    search = BH.mkSearch (Just query) Nothing

searchMain :: MonadIO m => m ()
searchMain = do
  args <- map toText <$> getArgs
  case args of
    ["--parse", query] -> putTextLn $ parseQuery query
    [elkUrl, index, query] -> runQuery elkUrl index query
    _ -> putTextLn "usage: elk-url index query"
