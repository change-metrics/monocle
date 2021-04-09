{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lentille.Client
import Lentille.Mock
import Lentille.Prelude
import Network.HTTP.Mock (withMockedManager)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain (testGroup "Tests" [clientTests])

clientTests :: TestTree
clientTests =
  testGroup
    "Lentille.Client"
    [testGetIndices]

withMockClient :: (MonocleClient -> IO ()) -> IO ()
withMockClient cb = withMockedManager monocleMockApplication go
  where
    go manager = withClient "http://localhost" (Just manager) cb

testGetIndices :: TestTree
testGetIndices = testCase "getIndices" go
  where
    go = withMockClient $ \client -> do
      indices <- getIndices client
      assertBool "Got indicies" (indices == ["indice1", "indice2"])
