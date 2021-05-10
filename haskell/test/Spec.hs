{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Google.Protobuf.Timestamp
import Monocle.Client
import Monocle.Mock
import Monocle.TaskData
import Monocle.WebApi
import Relude
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain (testGroup "Tests" [monocleWebApiTests])

monocleWebApiTests :: TestTree
monocleWebApiTests =
  testGroup
    "Monocle.WebApi"
    [ _taskDataGetLastUpdated
    ]
  where
    _taskDataGetLastUpdated =
      testCase
        "taskDataGetLastUpdated"
        ( test
            taskDataGetLastUpdated
            taskDataGetLastUpdatedInput
            taskDataGetLastUpdatedOutput
        )
    taskDataGetLastUpdatedInput = TaskDataGetLastUpdatedRequest "test" "test"
    taskDataGetLastUpdatedOutput =
      TaskDataGetLastUpdatedResponse
        { taskDataGetLastUpdatedResponseResult =
            Just
              ( TaskDataGetLastUpdatedResponseResultTimestamp
                  (Timestamp {timestampSeconds = 1609459200, timestampNanos = 0})
              )
        }

    test api input output = withMockClient withClient $ \client -> do
      resp <- api client input
      assertEqual "Response differ: " output resp
