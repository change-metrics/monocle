-- | This application evaluate different strategies to handle elasticsearch search result.
-- The goal is to compute the average age of a ChangeEvents (created_at - on_created) as
-- this seems to be a bottleneck of the changes review stats query.
--
-- Create a dataset by running: curl http://localhost:9200/index/_search -d '{todo: add type term query}' > data/sample.json
-- Then run the benchmark using `FP=$(pwd)/data/sample.json cabal bench`
module Main where

import Criterion.Main
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Bytes as B
import qualified Data.Text.Short as TextShort
import qualified Data.Text.Time as T
import qualified Database.Bloodhound as BH
import qualified Json as Json
import Monocle.Backend.Documents as D
import Monocle.Prelude
import System.Environment (getEnv)

-- | Test compute metric with Json
jsonPerf :: LBS.ByteString -> Int
jsonPerf dat = do
  case Json.decode (B.fromByteString (toStrict dat)) of
    Left e -> error (show e)
    Right v -> fromMaybe (error "failed!") (go v)
  where
    getValue k v = case v of
      Json.Object xs -> getFirst $ foldMap getValue' xs
      _ -> Nothing
      where
        getValue' Json.Member {..}
          | key == k = First $ Just value
          | otherwise = First Nothing
    getArray :: Json.Value -> Maybe [Json.Value]
    getArray v = case v of
      Json.Array xs -> pure $ toList xs
      _ -> Nothing
    getDate :: Json.Value -> UTCTime
    getDate v = case v of
      Json.String t -> either (error "oops") id $ T.parseUTCTimeOrError (TextShort.toText t)
      _ -> error "Not a date"
    toDuration :: Json.Value -> Maybe Pico
    toDuration v' = do
      v <- getValue "_source" v'
      createdAt <- getDate <$> getValue "created_at" v
      onCreatedAt <- getDate <$> getValue "on_created_at" v
      pure $ elapsedSeconds onCreatedAt createdAt
    go :: Json.Value -> Maybe Int
    go e = do
      hits <- getArray =<< getValue "hits" =<< getValue "hits" e
      avg <- average $ ((fromMaybe (error "bad hit") . toDuration) <$> hits)
      pure $ truncate avg

-- | Test decoding Json.Value
jsonDecode :: LBS.ByteString -> Int
jsonDecode dat = do
  case Json.decode (B.fromByteString (toStrict dat)) of
    Left e -> error (show e)
    Right _ -> 42

-- | Test decoding simple Aeson.Value
aesonDecode :: LBS.ByteString -> Int
aesonDecode dat = do
  getValue (Aeson.eitherDecode' dat)
  where
    getValue :: Either String (BH.SearchResult Aeson.Value) -> Int
    getValue (Right _) = 42
    getValue _ = error "oops"

-- | Test decoding the full data type
aesonDecodeData :: LBS.ByteString -> Int
aesonDecodeData dat =
  getE (Aeson.eitherDecode dat)
  where
    getE :: Either String (BH.SearchResult D.EChangeEvent) -> Int
    getE (Right _) = 42
    getE _ = error "oops"

getData :: IO LBS.ByteString
getData = do
  fp <- getEnv "FP"
  LBS.readFile fp

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["print"] -> print . jsonPerf =<< getData
    _ -> do
      defaultMain
        [ env getData $ \dat ->
            bgroup
              "main"
              [ bgroup
                  "duration"
                  [bench "json" $ whnf jsonPerf dat],
                bgroup
                  "decode"
                  [ bench "decode" $ whnf jsonDecode dat,
                    bench "aeson-value" $ whnf aesonDecode dat,
                    bench "aeson-elastic" $ whnf aesonDecodeData dat
                  ]
              ]
        ]
