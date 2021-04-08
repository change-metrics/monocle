{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Copyright: (c) 2021 Monocle authors
-- SPDX-License-Identifier: AGPL-3.0-only
-- Maintainer: Monocle authors <fboucher@redhat.com>
--
-- Utility functions and re-exports
module Lentille.Prelude
  ( MonadThrow,
    UTCTime,
    log,
    getYesterday,
    module Relude,
  )
where

import Control.Monad.Catch (MonadThrow)
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Relude

log :: MonadIO m => Text -> m ()
log msg = do
  now <- liftIO getCurrentTime
  putTextLn $ "[" <> show now <> "]: " <> msg

getYesterday :: MonadIO m => m UTCTime
getYesterday = addUTCTime (-24 * 3600) <$> liftIO getCurrentTime
