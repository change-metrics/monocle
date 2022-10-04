module Monocle.Servant.HTTPAuth where

import Data.Text
import Lucid (Html)
import Monocle.Api.Jwt (LoginInUser (..))
import Monocle.Servant.HTTP
import Servant
import Servant.HTML.Blaze qualified as B (HTML)
import Servant.HTML.Lucid (HTML)

-- | The API is served at both `/api/2/` (for backward compat with the legacy nginx proxy)
-- and `/` (for compat with crawler client)
type MonocleAPI' = MonocleAPI :<|> HtmxAPI :<|> AuthAPI

type RootAPI = "api" :> "2" :> MonocleAPI' :<|> MonocleAPI'

type AuthAPI =
  "auth" :> "login" :> QueryParam "redirectUri" Text :> Get '[JSON] NoContent
    :<|> "auth" :> "cb" :> QueryParam "error" Text :> QueryParam "code" Text :> QueryParam "state" Text :> Get '[B.HTML] LoginInUser

type HtmxAPI =
  "htmx" Servant.:> "authors_search" :> QueryParam "search" Text :> Get '[HTML] (Html ())
