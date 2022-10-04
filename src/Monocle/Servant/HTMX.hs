module Monocle.Servant.HTMX where

import Data.Text
import Lucid (Html)
import Servant
import Servant.HTML.Lucid (HTML)

type HtmxAPI =
  "htmx" :> "authors_search" :> QueryParam "index" Text :> QueryParam "search" Text :> Get '[HTML] (Html ())
