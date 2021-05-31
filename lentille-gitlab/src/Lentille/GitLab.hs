
-- TMP
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Lentille.GitLab where
import Relude
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS (tlsManagerSettings)

schemaLocation :: String
schemaLocation = "./gitlab-schema/schema.graphql"

-------------------------------------------------------------------------------
-- HTTP Client
-------------------------------------------------------------------------------
data GitLabGraphClient = GitLabGraphClient
  { manager :: HTTP.Manager,
    url :: Text
  }

newGitLabGraphClient :: MonadIO m => Text -> m GitLabGraphClient
newGitLabGraphClient url' = do
  manager' <- liftIO $ HTTP.newManager tlsManagerSettings
  pure $ GitLabGraphClient manager' url'

