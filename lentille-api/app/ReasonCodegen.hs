{-# LANGUAGE NoImplicitPrelude #-}

-- |
module Main (main) where

import Lentille.Api (api)
import Relude
import Servant.Reason (generateReasonForAPI)

main :: IO ()
main = do
  mapM_ putTextLn $ generateReasonForAPI api
