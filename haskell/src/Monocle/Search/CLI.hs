-- | monocle-search CLI entry point
module Monocle.Search.CLI (searchMain) where

import qualified Data.Aeson as Aeson
import Monocle.Api.Config (defaultTenant)
import qualified Monocle.Backend.Queries as Q
import Monocle.Env (runQueryM, testTenantM)
import Monocle.Prelude
import qualified Monocle.Search.Parser as P
import qualified Monocle.Search.Query as Q

parseQuery :: UTCTime -> Text -> Text
parseQuery now code = either show decodeUtf8 queryJson
  where
    queryJson = do
      expr <- P.parse [] code
      query <- Q.queryWithMods now mempty Nothing expr
      pure $ Aeson.encode $ Q.queryGet query id Nothing

printQuery :: MonadIO m => UTCTime -> Text -> Text -> m ()
printQuery now index code = do
  changes <- liftIO $ testTenantM (defaultTenant index) $ runQueryM query $ Q.changes Nothing 10
  mapM_ (putTextLn . show) (take 2 changes)
  putTextLn $ "Got : " <> show (length changes) <> " results"
  where
    query = case P.parse [] code >>= Q.queryWithMods now mempty Nothing of
      Left err -> error $ "Invalid query: " <> show err
      Right q -> q

searchMain :: MonadIO m => m ()
searchMain = do
  now <- getCurrentTime
  args <- map toText <$> getArgs
  case args of
    ["--parse", query] -> putTextLn $ parseQuery now query
    [index, code] -> printQuery now index code
    _otherArgs -> putTextLn "usage: index query"
