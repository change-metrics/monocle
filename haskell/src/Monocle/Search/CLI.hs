{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | monocle-search CLI entry point
module Monocle.Search.CLI (searchMain) where

import qualified Data.Aeson as Aeson
import qualified Monocle.Backend.Index as I
import qualified Monocle.Backend.Queries as Q
import Monocle.Backend.Test (emptyConfig)
import Monocle.Prelude
import qualified Monocle.Search.Parser as P
import qualified Monocle.Search.Query as Q
import Monocle.Servant.Env (runTenantM')

parseQuery :: UTCTime -> Text -> Text
parseQuery now code = either show decodeUtf8 query
  where
    query = do
      expr <- P.parse code
      Aeson.encode . Q.queryBH <$> Q.queryWithMods now mempty Nothing expr

printQuery :: MonadIO m => UTCTime -> Text -> Text -> Text -> m ()
printQuery now elkUrl index code = do
  bhEnv <- I.mkEnv elkUrl
  changes <- liftIO $ runTenantM' bhEnv (emptyConfig index) $ Q.changes query
  mapM_ (putTextLn . show) (take 2 changes)
  putTextLn $ "Got : " <> show (length changes) <> " results"
  where
    query = case P.parse code >>= Q.queryWithMods now mempty Nothing of
      Left err -> error $ "Invalid query: " <> show err
      Right q -> q

searchMain :: MonadIO m => m ()
searchMain = do
  now <- liftIO getCurrentTime
  args <- map toText <$> getArgs
  case args of
    ["--parse", query] -> putTextLn $ parseQuery now query
    [elkUrl, index, code] -> printQuery now elkUrl index code
    _otherArgs -> putTextLn "usage: elk-url index query"
