{-# LANGUAGE NoImplicitPrelude #-}

-- |
module Main (main) where

import Lentille.Api (app)
import Network.Wai.Handler.Warp (run)
import Relude

main :: IO ()
main = run 3042 =<< app
