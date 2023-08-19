module Monocle.Servant.HTTPMain where

import Data.Text
import Monocle.Api.Jwt (LoginInUser (..))
import Monocle.Servant.HTMX
import Monocle.Servant.HTTP
import Servant
import Servant.Auth.Server (SetCookie)
import Servant.HTML.Blaze (HTML)

import Monocle.Butler (ButlerHtmlAPI, ButlerWSAPI)

-- | The API is served at both `/api/2/` (for backward compat with the legacy nginx proxy)
-- and `/` (for compat with crawler client)
type MonocleAPI' = MonocleAPI :<|> HtmxAPI :<|> AuthAPI

type RootAPI = "api" :> "2" :> MonocleAPI' :<|> MonocleAPI' :<|> ButlerHtmlAPI :<|> ButlerWSAPI

type AuthAPI =
  "auth" :> "login" :> QueryParam "redirectUri" Text :> Get '[JSON] NoContent
    :<|> "auth"
      :> "cb"
      :> QueryParam "error" Text
      :> QueryParam "code" Text
      :> QueryParam "state" Text
      :> Get
          '[HTML]
          ( Headers
              '[ Header "Set-Cookie" SetCookie
               , Header "Set-Cookie" SetCookie
               , Header "Set-Cookie" SetCookie
               ]
              LoginInUser
          )
