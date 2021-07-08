module Main (main) where

import Test.DocTest

main :: IO ()
main = doctest (opts <> ["-isrc", "src/"])
  where
    opts :: [String]
    opts =
      map
        (mappend "-X")
        [ "BangPatterns",
          "DeriveGeneric",
          "DerivingStrategies",
          "GeneralizedNewtypeDeriving",
          "LambdaCase",
          "MultiWayIf",
          "NamedFieldPuns",
          "NamedWildCards",
          "NoImplicitPrelude",
          "NumDecimals",
          "NumericUnderscores",
          "OverloadedStrings",
          "PatternSynonyms",
          "RecordWildCards",
          "ScopedTypeVariables",
          "TypeApplications",
          "TypeOperators"
        ]
