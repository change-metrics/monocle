{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
module Monocle.Backend.Index where

import Data.Aeson
import qualified Data.Vector as V
import qualified Database.Bloodhound as BH
import Relude

data Change = Change
  { name :: Text,
    author :: Text,
    changeId :: Text
  }
  deriving (Show, Eq)

instance ToJSON Change where
  toJSON Change {..} =
    object
      [ "name" .= name,
        "author" .= author
      ]

indexChanges :: BH.BHEnv -> BH.IndexName -> [Change] -> IO ()
indexChanges bhEnv index changes = BH.runBH bhEnv $ do
  let stream = V.fromList (toBulkIndex <$> changes)
  _ <- BH.bulk stream
  -- Bulk loads require an index refresh before new data is loaded.
  _ <- BH.refreshIndex index
  pure ()
  where
    toBulkIndex change =
      BH.BulkIndex index (BH.DocId $ changeId change) (toJSON change)
