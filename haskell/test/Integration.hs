{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Monocle.Backend.Test (testIndexChange)
import Relude
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain (testGroup "Tests" [monocleIntegrationTests])

monocleIntegrationTests :: TestTree
monocleIntegrationTests =
  testGroup
    "Monocle.Backend.Changes"
    [ testCase "Index changes" testIndexChange
    ]
