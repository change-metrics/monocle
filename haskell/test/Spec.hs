{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import qualified Data.Aeson as Aeson
import Google.Protobuf.Timestamp
import Monocle.Client
import Monocle.Mock
import qualified Monocle.Search.Lexer as L
import qualified Monocle.Search.Parser as P
import qualified Monocle.Search.Query as Q
import qualified Monocle.Search.Syntax as S
import Monocle.TaskData
import Monocle.WebApi
import Relude
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain (testGroup "Tests" [monocleSearchLanguage, monocleWebApiTests])

monocleSearchLanguage :: TestTree
monocleSearchLanguage =
  testGroup
    "Monocle.Search"
    [ testCase
        "Lexer basic"
        ( lexMatch "state:open" [L.Literal "state", L.Equal, L.Literal "open"]
        ),
      testCase
        "Parser basic"
        (parseMatch "state:open" (S.EqExpr "state" "open")),
      testCase
        "Lexer paren"
        ( lexMatch
            "(a>42 or a = 0) and b: d"
            [ L.OpenParenthesis,
              L.Literal "a",
              L.Greater,
              L.Literal "42",
              L.Or,
              L.Literal "a",
              L.Equal,
              L.Literal "0",
              L.CloseParenthesis,
              L.And,
              L.Literal "b",
              L.Equal,
              L.Literal "d"
            ]
        ),
      testCase
        "Lexer quoted"
        (lexMatch "field:\"A value\"" [L.Literal "field", L.Equal, L.Literal "A value"]),
      testCase
        "Parser paren"
        ( parseMatch
            "(a>42 or a:0) and b:d"
            ( (S.AndExpr (S.OrExpr (S.GtExpr "a" "42") (S.EqExpr "a" "0")) (S.EqExpr "b" "d"))
            )
        ),
      testCase
        "Parser order by"
        ( parseMatch
            "state:open order by review_date"
            (S.OrderByExpr "review_date" S.Asc (S.EqExpr "state" "open"))
        ),
      testCase
        "Parser order by sort"
        ( parseMatch
            "state:open order by review_date desc"
            (S.OrderByExpr "review_date" S.Desc (S.EqExpr "state" "open"))
        ),
      testCase
        "Query date"
        ( queryMatch
            "updated_at>2021-05-27"
            "{\"range\":{\"updated_at\":{\"gt\":\"2021-05-27T00:00:00Z\",\"boost\":1}}}"
        ),
      testCase
        "Query number"
        ( queryMatch
            "score>200"
            "{\"range\":{\"tasks_data.score\":{\"gt\":200,\"boost\":1}}}"
        ),
      testCase
        "Query boolean"
        ( queryMatch
            "self_merged:false"
            "{\"term\":{\"self_merged\":{\"value\":\"false\"}}}"
        ),
      testCase
        "Query regex"
        ( queryMatch
            "repo_regex:openstack/.*nova.*"
            "{\"regexp\":{\"repository_fullname\":{\"flags\":\"ALL\",\"value\":\"openstack/.*nova.*\"}}}"
        ),
      testCase
        "Query state"
        ( queryMatch
            "state: abandoned"
            "{\"term\":{\"state\":{\"value\":\"CLOSED\"}}}"
        ),
      testCase
        "Query date"
        ( queryMatch
            "updated_at > 2021 and updated_at < 2021-05"
            "{\"bool\":{\"must\":[{\"range\":{\"updated_at\":{\"gt\":\"2021-01-01T00:00:00Z\",\"boost\":1}}},{\"range\":{\"updated_at\":{\"lt\":\"2021-05-01T00:00:00Z\",\"boost\":1}}}]}}"
        )
    ]
  where
    lexMatch code tokens = assertEqual "match" (Right tokens) (fmap L.token <$> L.lex code)
    parseMatch code expr = assertEqual "match" (Right expr) (P.parse code)
    queryMatch code query =
      assertEqual
        "match"
        (Right query)
        (P.parse code >>= Q.queryWithMods >>= pure . Aeson.encode . Q.queryBH)

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
