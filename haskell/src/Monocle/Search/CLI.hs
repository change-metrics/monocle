{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | monocle-search CLI entry point
module Monocle.Search.CLI (searchMain) where

import qualified Data.Aeson as Aeson
import qualified Monocle.Search.Parser as P
import qualified Monocle.Search.Queries as Q
import qualified Monocle.Search.Query as Q
import Relude

parseQuery :: Text -> Text
parseQuery code = either show decodeUtf8 query
  where
    query = do
      expr <- P.parse code
      Aeson.encode . Q.queryBH <$> Q.queryWithMods expr

printQuery :: MonadIO m => Text -> Text -> Text -> m ()
printQuery elkUrl index code = do
  bhEnv <- Q.mkEnv elkUrl
  changes <- Q.changes bhEnv index query
  mapM_ (putTextLn . show) (take 2 changes)
  putTextLn $ "Got : " <> show (length changes) <> " results"
  where
    query = case P.parse code >>= Q.queryWithMods of
      Left err -> error $ "Invalid query: " <> show err
      Right q -> q

searchMain :: MonadIO m => m ()
searchMain = do
  args <- map toText <$> getArgs
  case args of
    ["--parse", query] -> putTextLn $ parseQuery query
    [elkUrl, index, code] -> printQuery elkUrl index code
    _ -> putTextLn "usage: elk-url index query"
