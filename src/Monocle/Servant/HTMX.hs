module Monocle.Servant.HTMX where

import Data.Text
import Lucid (Html)
import Monocle.Api.Jwt (AuthenticatedUser)
import Monocle.Config qualified as Config
import Servant
import Servant.Auth.Server
import Servant.HTML.Lucid (HTML)

type HtmxAPI =
  "htmx"
    :> "authors_search"
    :> Auth '[JWT, Cookie] AuthenticatedUser
    :> QueryParam "index" Config.IndexName
    :> QueryParam "search" Text
    :> Get '[HTML] (Html ())
