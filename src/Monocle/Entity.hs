module Monocle.Entity (
  Entity (..),
  entityTypeName,
  entityDocID,
) where

import Data.Text qualified

import Monocle.Prelude
import Monocle.Protob.Crawler qualified as CrawlerPB

-- | Entity.
data Entity
  = Project Text
  | Organization Text
  | TaskDataEntity Text
  deriving (Eq, Show)

entityTypeName :: CrawlerPB.EntityType -> Text
entityTypeName = \case
  CrawlerPB.EntityTypeENTITY_TYPE_PROJECT -> "project"
  CrawlerPB.EntityTypeENTITY_TYPE_ORGANIZATION -> "organization"
  CrawlerPB.EntityTypeENTITY_TYPE_TASK_DATA -> "taskdata"

-- TODO: check if the value needs to be hashed to prevent escape issue
entityDocID :: Entity -> Text
entityDocID e = entityTypeName (from e) <> "-" <> cleanName
 where
  cleanName = Data.Text.replace "/" "@" $
    case e of
      Project n -> n
      Organization n -> n
      TaskDataEntity n -> n

instance From Entity CrawlerPB.Entity where
  from e = CrawlerPB.Entity (Just pbe)
   where
    pbe = case e of
      Project n -> CrawlerPB.EntityEntityProjectName (from n)
      Organization n -> CrawlerPB.EntityEntityOrganizationName (from n)
      TaskDataEntity n -> CrawlerPB.EntityEntityTdName (from n)

instance From Entity CrawlerPB.EntityType where
  from = \case
    Project _ -> CrawlerPB.EntityTypeENTITY_TYPE_PROJECT
    Organization _ -> CrawlerPB.EntityTypeENTITY_TYPE_ORGANIZATION
    TaskDataEntity _ -> CrawlerPB.EntityTypeENTITY_TYPE_TASK_DATA
