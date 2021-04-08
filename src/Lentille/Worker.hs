{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Copyright: (c) 2021 Monocle authors
-- SPDX-License-Identifier: AGPL-3.0-only
-- Maintainer: Monocle authors <fboucher@redhat.com>
--
-- The Monocle worker
module Lentille.Worker
  ( run,
  )
where

import Lentille.Client (MonocleClient, TasksSummary (..), getTasksSummary)
import Lentille.Prelude

-- | A Worker is defined by the runTT function
newtype Worker m = Worker {runWorker :: UTCTime -> TasksSummary -> m UpdateFlag}

data UpdateFlag = Updated | NotUpdated

notUpdated :: (UpdateFlag, TasksSummary) -> Bool
notUpdated (NotUpdated, _) = False
notUpdated _ = True

githubTasks :: MonadIO m => Worker m
githubTasks = Worker getGithubUpdates
  where
    getGithubUpdates :: MonadIO m => UTCTime -> TasksSummary -> m UpdateFlag
    getGithubUpdates sinceTS tasksSummary =
      if sinceTS >= lastUpdated
        then do
          log $ "Getting updates from " <> ts_url tasksSummary
          pure Updated
        else pure NotUpdated
      where
        lastUpdated = fromMaybe sinceTS (ts_last_updated tasksSummary)

run :: (MonadThrow m, MonadIO m) => MonocleClient -> UTCTime -> m ()
run monocleClient sinceTS = do
  log "Starting updates"
  tasks <- getTasksSummary monocleClient
  updated <- mapM (runWorker githubTasks sinceTS) tasks
  log "Update summary:"
  mapM_ (putTextLn . mappend "- " . show . snd) $ filter notUpdated (zip updated tasks)
