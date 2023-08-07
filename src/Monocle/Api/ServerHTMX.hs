module Monocle.Api.ServerHTMX where

import Data.String.Interpolate (iii)
import Data.Vector qualified as V
import Database.Bloodhound qualified as BH
import Lucid
import Lucid.Base
import Monocle.Api.Jwt (AuthenticatedUser)
import Monocle.Api.Server (searchAuthor)
import Monocle.Backend.Documents (EDocType (ECachedAuthor))
import Monocle.Backend.Queries (documentType)
import Monocle.Effects (ApiEffects, esCountByIndex)
import Monocle.Env (tenantIndexName)
import Monocle.Prelude
import Monocle.Protob.Search (AuthorRequest (..))
import Monocle.Protob.Search qualified as SearchPB
import Servant.Auth.Server

hxGet, hxTrigger, hxTarget, hxVals :: Text -> Attribute
hxGet = makeAttribute "hx-get"
hxTrigger = makeAttribute "hx-trigger"
hxTarget = makeAttribute "hx-target"
hxVals = makeAttribute "hx-vals"

searchAuthorsHandler :: ApiEffects es => AuthResult AuthenticatedUser -> Maybe Text -> Maybe Text -> Eff es (Html ())
searchAuthorsHandler _ Nothing _ = pure $ pure ()
searchAuthorsHandler auth (Just index) queryM = do
  case queryM of
    Just query -> do
      (SearchPB.AuthorResponse results) <-
        searchAuthor auth (AuthorRequest (from index) (from query))
      case toList results of
        [] -> pure $ div_ "No Results"
        _xs -> pure $ mapM_ authorToMarkup results
    Nothing -> do
      totalCount <- countCachedAuthors
      pure $ div_ [class_ "pf-c-card__body"] $ do
        div_ [class_ "pf-l-stack pf-m-gutter"] $ do
          div_ [class_ "pf-l-stack__item"] $ do
            div_ $ show totalCount <> " author(s) available in the database"
          div_ [class_ "pf-l-stack__item"] $ do
            div_ [id_ "search-input"]
              $ input_
                [ type_ "text"
                , name_ "search"
                , class_ "pf-c-form-control"
                , placeholder_ "Start typing to search authors"
                , hxGet "/htmx/authors_search"
                , hxVals [iii|{"index": "#{index}"}|]
                , hxTrigger "keyup changed delay:500ms, search"
                , hxTarget "#search-results"
                ]
          div_ [class_ "pf-l-stack__item"] $ do
            div_ [id_ "search-results"] ""
          script_ pushToRouter
 where
  countCachedAuthors = do
    resp <- esCountByIndex (tenantIndexName index) $ BH.CountQuery $ documentType ECachedAuthor
    case resp of
      Right (BH.CountResponse nat _) -> pure nat
      Left _ -> pure 0

  pushToRouter =
    [iii|function pushToRouter(index, entity, name) {
        return RescriptReactRouter.push("/" + index + "/" + entity + "/" + encodeURIComponent(name))};
    |]
  authorToMarkup :: SearchPB.Author -> Html ()
  authorToMarkup (SearchPB.Author muid aliases groups) = do
    div_ [class_ "pf-c-card pf-m-compact"] $ do
      div_ [class_ "pf-c-card__body"] $ do
        div_ [class_ "pf-l-flex"] $ do
          a_ [onclick_ clickAuthorF] $ toHtml muid
          unless (V.null aliases) aliasesH
          unless (V.null groups) groupH
   where
    clickAuthorF = [iii|pushToRouter("#{index}","author","#{muid}")|]
    clickGroupF groupName = [iii|pushToRouter("#{index}","group","#{groupName}")|]
    aliasesH = do
      hr_ [class_ "pf-c-divider pf-m-vertical"]
      ul_ [class_ "pf-c-list pf-m-inline"] $ do
        mapM_ (li_ . toHtml) aliases
    groupH = do
      hr_ [class_ "pf-c-divider pf-m-vertical"]
      ul_ [class_ "pf-c-list pf-m-inline"] $ do
        mapM_ (\g -> li_ . a_ [onclick_ $ clickGroupF g] $ toHtml g) groups
