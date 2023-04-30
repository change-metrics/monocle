-- | This application evaluate different strategies to handle elasticsearch search result.
-- The goal is to compute the average age of a ChangeEvents (created_at - on_created) as
-- this seems to be a bottleneck of the changes review stats query.
module Main where

import Criterion.Main
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Bytes qualified as B
import Data.Text.Short qualified as TextShort
import Data.Text.Time qualified as T
import Database.Bloodhound qualified as BH
import Faker qualified
import Faker.Combinators qualified
import Json qualified as Json
import Monocle.Backend.Documents as D
import Monocle.Backend.Provisioner (fakeChangeEvent)
import Monocle.Prelude
import System.Random (mkStdGen)

-- | Test compute metric with Json
jsonPerf :: LBS.ByteString -> Int
jsonPerf dat = do
  case Json.decode (B.fromByteString (from dat)) of
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
  case Json.decode (B.fromByteString (from dat)) of
    Left e -> error (show e)
    Right _ -> 42

-- | Test decoding simple Aeson.Value
aesonDecode :: LBS.ByteString -> Int
aesonDecode dat = do
  getValue (Aeson.eitherDecode' dat)
 where
  getValue :: Either String (BH.SearchResult Aeson.Value) -> Int
  getValue (Right _) = 42
  getValue x = error (show x)

-- | Test decoding the full data type
aesonDecodeData :: LBS.ByteString -> Int
aesonDecodeData dat =
  getE (Aeson.eitherDecode dat)
 where
  getE :: Either String (BH.SearchResult D.EChangeEvent) -> Int
  getE (Right _) = 42
  getE x = error (show x)

getJsonData :: IO LBS.ByteString
getJsonData = do
  xs <-
    Faker.generateWithSettings (Faker.setRandomGen stdGen $ Faker.setNonDeterministic Faker.defaultFakerSettings) $
      Faker.Combinators.listOf total (fakeChangeEvent minDate maxDate)
  pure $ Aeson.encode $ mkObj xs
 where
  minDate = [utctime|1970-01-01 00:00:00|]
  maxDate = [utctime|2021-01-01 00:00:00|]

  stdGen = mkStdGen 42
  mkObj xs =
    Aeson.object
      [ "took" .= toJSON (4 :: Int)
      , "timed_out" .= toJSON False
      , "_shards"
          .= Aeson.object
            [ "total" .= toJSON (1 :: Int)
            , "successful" .= toJSON (1 :: Int)
            , "skipped" .= toJSON (0 :: Int)
            , "failed" .= toJSON (0 :: Int)
            ]
      , "hits"
          .= Aeson.object
            [ "total" .= Aeson.object ["value" .= (toJSON total), "relation" .= ("eq" :: Text)]
            , "max_score" .= toJSON (1 :: Float)
            , "hits" .= (Aeson.Array $ fromList $ map mkDoc xs)
            ]
      ]
  total = 10000
  mkDoc xs =
    Aeson.object
      [ "_index" .= ("test" :: Text)
      , "_type" .= ("_doc" :: Text)
      , "_id" .= ("fake" :: Text)
      , "_score" .= toJSON (1 :: Float)
      , "_source" .= toJSON xs
      ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["print"] -> putTextLn . decodeUtf8 =<< getJsonData
    _ -> do
      defaultMain
        [ env getJsonData $ \dat ->
            bgroup
              "main"
              [ bgroup
                  "duration"
                  [bench "json" $ whnf jsonPerf dat]
              , bgroup
                  "decode"
                  [ bench "decode" $ whnf jsonDecode dat
                  , bench "aeson-value" $ whnf aesonDecode dat
                  , bench "aeson-elastic" $ whnf aesonDecodeData dat
                  ]
              ]
        ]
