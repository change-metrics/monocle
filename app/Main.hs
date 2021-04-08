{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lentille

-- TODO: implement proper CLI
main :: IO ()
main = withClient "https://demo.changemetrics.io/" $ \client -> do
  ts <- getYesterday
  run client ts
