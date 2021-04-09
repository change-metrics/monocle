{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
module Lentille.Mock where

import Data.Aeson (encode)
import Network.HTTP.Types.Status (status200)
import qualified Network.Wai as Wai
import Relude

fakeIndices :: [Text]
fakeIndices = ["indice1", "indice2"]

monocleMockApplication :: Wai.Application
monocleMockApplication _req respond = do
  respond $ Wai.responseLBS status200 [("Content-Type", "application/json")] (encode fakeIndices)
