{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
module Main (main) where

import Lentille.App (app)
import Network.Wai.Handler.Warp (run)
import Relude

main :: IO ()
main = do
  [portStr] <- getArgs
  let port = fromMaybe 3000 (readMaybe portStr)
  putTextLn $ "Serving api http://localhost:" <> show port
  run port =<< app
