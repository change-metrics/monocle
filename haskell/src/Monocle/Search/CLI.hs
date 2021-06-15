{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | monocle-search CLI entry point
module Monocle.Search.CLI (searchMain) where

import qualified Data.Aeson as Aeson
import Data.Time.Clock (UTCTime, getCurrentTime)
import qualified Monocle.Backend.Index as I
import qualified Monocle.Backend.Queries as Q
import qualified Monocle.Search.Parser as P
import qualified Monocle.Search.Query as Q
import Relude

parseQuery :: UTCTime -> Text -> Text
parseQuery now code = either show decodeUtf8 query
  where
    query = do
      expr <- P.parse code
      Aeson.encode . Q.queryBH <$> Q.queryWithMods now expr

printQuery :: MonadIO m => UTCTime -> Text -> Text -> Text -> m ()
printQuery now elkUrl index code = do
  bhEnv <- I.mkEnv elkUrl
  changes <- Q.changes bhEnv index query
  mapM_ (putTextLn . show) (take 2 changes)
  putTextLn $ "Got : " <> show (length changes) <> " results"
  where
    query = case P.parse code >>= Q.queryWithMods now of
      Left err -> error $ "Invalid query: " <> show err
      Right q -> q

searchMain :: MonadIO m => m ()
searchMain = do
  now <- liftIO getCurrentTime
  args <- map toText <$> getArgs
  case args of
    ["--parse", query] -> putTextLn $ parseQuery now query
    [elkUrl, index, code] -> printQuery now elkUrl index code
    _ -> putTextLn "usage: elk-url index query"
