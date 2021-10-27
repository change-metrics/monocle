{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Main (main) where

import qualified Lentille.Gerrit as G
import Monocle.Prelude
import Monocle.Search.Query (parseDateValue)
import Options.Generic
import qualified Streaming.Prelude as S

data Lentille
  = GerritChange {url :: Text, change :: Int}
  | GerritProjects {url :: Text, query :: Text}
  | GerritChanges {url :: Text, project :: Text, since :: Text, limit :: Maybe Int}
  deriving stock (Generic)

instance ParseRecord Lentille where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

dump :: (MonadIO m, ToJSON a) => Maybe Int -> Stream (Of a) m () -> m ()
dump limitM stream = do
  xs <- S.toList_ $ brk $ stream
  liftIO . putBSLn . from . encodePrettyWithSpace 2 $ xs
  where
    brk = case limitM of
      Nothing -> id
      Just limit -> S.take limit

main :: IO ()
main = do
  args <- getRecord "Lentille runner"
  case args of
    GerritChange url change -> do
      env <- getGerritEnv url
      dump Nothing $ G.streamChange env [G.ChangeId $ show change]
    GerritProjects url query -> do
      env <- getGerritEnv url
      dump Nothing $ G.streamProject env $ G.Regexp query
    GerritChanges url project since limit -> do
      env <- getGerritEnv url
      dump limit $ G.streamChange env [G.Project project, G.After (toSince since)]
  where
    toSince txt = case Monocle.Search.Query.parseDateValue txt of
      Just x -> x
      Nothing -> error $ "Invalid date: " <> show txt
    getGerritEnv url = do
      client <- G.getGerritClient url Nothing
      pure $ G.GerritEnv client Nothing (const Nothing)
