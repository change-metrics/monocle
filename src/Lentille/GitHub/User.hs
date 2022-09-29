{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Lentille.GitHub.User where

import Data.Morpheus.Client
import Lentille.GitHub.RateLimit (retryCheck)
import Lentille.GraphQL
import Monocle.Prelude

newtype DateTime = DateTime Text deriving (Show, Eq, EncodeScalar, DecodeScalar)

newtype URI = URI Text deriving (Show, Eq, EncodeScalar, DecodeScalar)

data IdentInfo = IdentInfo
  { iiAvatarUrl :: Text
  , iiName :: Maybe Text
  , iiCompany :: Maybe Text
  , iiLocation :: Maybe Text
  , iiOrganizations :: [Text]
  }
  deriving (Show)

-- https://docs.github.com/en/graphql/reference/objects#user
-- To get the orgs login the following scope: ['read:org'] is required
defineByDocumentFile
  ghSchemaLocation
  [gql|
    query getUser ($login: String!) {
      rateLimit {
        used
        remaining
        resetAt
      }
      user(login: $login) {
        avatarUrl
        name
        company
        location
        organizations (first: 100) {
          nodes {
            orgLogin: login
          }
        }
      }
    }
  |]

transformResponse :: GraphResp GetUser -> (RateLimit, IdentInfo)
transformResponse (Left err) = error (show err)
transformResponse (Right resp) = case resp of
  GetUser
    (Just (RateLimitRateLimit used remaining (DateTime resetAtText)))
    ( Just
        ( UserUser
            (URI iiAvatarUrl)
            iiName
            iiCompany
            iiLocation
            (UserOrganizationsOrganizationConnection (Just orgListM))
          )
      ) ->
      let rateLimit = case parseDateValue $ from resetAtText of
            Just resetAt -> RateLimit {..}
            Nothing -> error $ "Unable to parse the resetAt date string: " <> resetAtText
          iiOrganizations = orgLogin <$> catMaybes orgListM
       in (rateLimit, IdentInfo {..})
  respOther -> error ("Invalid response: " <> show respOther)

getUser :: GraphEffects es => GraphClient -> Text -> Eff es IdentInfo
getUser client login =
  do
    (_, info) <-
      transformResponse
        <$> doRequest client mkArgs retryCheck Nothing Nothing
    pure info
 where
  mkArgs _ _ = GetUserArgs login
