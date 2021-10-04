-- | Utility function to clean up and maintain the backend
module Monocle.Backend.Janitor (removeCrawlerData) where

-- import qualified Database.Bloodhound as BH
import qualified Monocle.Backend.Index as I
import Monocle.Backend.Queries as Q
import Monocle.Env
import Monocle.Prelude
import qualified Streaming.Prelude as Streaming

-- | Remove all the data associated with a crawler name
-- Try this on the REPL with:
-- λ> testTenantM (mkConfig "zuul") $ removeCrawlerData "custom"
removeCrawlerData :: Text -> TenantM ()
removeCrawlerData crawlerName = do
  tdDeletedCount <- removeTaskDatas
  monocleLog $ crawlerName <> ": deleted " <> show tdDeletedCount <> " td"
  where
    removeTaskDatas :: TenantM Int
    removeTaskDatas = I.bulkStream $ bulkDelete $ runQueryStream taskDataQuery Q.scanSearchId

    taskDataQuery = mkQuery [mkTerm "tasks_data.crawler_name" crawlerName]

    -- Helper to delete a stream of DocId
    bulkDelete :: Stream (Of DocId) TenantM () -> Stream (Of BulkOperation) TenantM ()
    bulkDelete s = do
      index <- lift getIndexName
      Streaming.map (BulkDelete index) s
