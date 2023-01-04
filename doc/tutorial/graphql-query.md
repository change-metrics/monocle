Add a new graphql query
=======================

This tutorial teachs you how to use the morpheus-graphql-client to create a graphql query.

# Create a new module

In a file named `src/Lentille/GitHub/Issues.hs`, add:

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Lentille.GitHub.Issues where

import qualified Data.ByteString.Lazy as LBS
import Lentille.GitHub
import Data.Morpheus.Client
import Relude
```

Then in the `lentille-github-query.cabal` file add the new module to the
`exposed-modules:` list.

Validate by starting a repl:

```ShellSession
$ cabal repl --build-depends pretty-simple
[1 of 3] Compiling Lentille.GitHub
[2 of 3] Compiling Lentille.GitHub.Favorites
[3 of 3] Compiling Lentille.GitHub.Issues
λ>
```

# Add your query

Add to the file your query like so:

```haskell
import Lentille.GitHub (schemaLocation)
declareLocalTypesInline
  schemaLocation
  [raw|
    { query content here }
  |]
```

In this tutorial, we will use this example:

```graphql
query GetIssues ($searchText: String!)
{
    search(query: $searchText, type: ISSUE, first: 25) {
        nodes {
            ... on Issue {
                title
                updatedAt
                timelineItems(
                  first: 100,
                  itemTypes: [CONNECTED_EVENT],
                ) {
                  nodes {
                    ... on ConnectedEvent {
                      subject {
                        ... on PullRequest {
                          url
                        }
                      }
                    }
                  }
               }
            }
        }
    }
}
```

Then reload the repl:

```ShellSession
λ> :reload
src/Lentille/GitHub/Issues.hs:18:1: error:
    Not in scope: type constructor or class ‘URI’
   |
18 | defineByDocumentFile
   | ^^^^^^^^^^^^^^^^^^^^...

src/Lentille/GitHub/Issues.hs:18:1: error:
    Not in scope: type constructor or class ‘DateTime’
   |
18 | defineByDocumentFile
   | ^^^^^^^^^^^^^^^^^^^^...
```

The error happens because the github schema define types that are unknown in haskell.

You can check the generated code by the defineByDocumentFile function by enabling this extension:

```ShesllSession
λ> :set -ddump-splices
λ> :reload
src/Lentille/GitHub/Issues.hs:(20,8)-(45,4): Splicing expression
    template-haskell-2.16.0.0:Language.Haskell.TH.Quote.quoteExp
    ..
```

As you can see, there are unknown type constructor like URI or DateTime. So add newtype for those:

```haskell
newtype DateTime = DateTime Text deriving (Show, Eq, FromJSON)

newtype URI = URI Text deriving (Show, Eq, FromJSON)
```

... then reload:

```haskell
λ> :reload
[3 of 3] Compiling Lentille.GitHub.Issues
Ok, three modules loaded.
```

At this point, you can generate the documentation to see the data types, in another terminal:

```ShellSession
$ cabal haddock
...
Documentation created:
/path/to/doc
$ python3 -m http.server  --directory /path/to/doc
```

# Test the morpheus-graphql-client fetch function

Add this function to give it a shot:

```haskell
fetchIssue :: MonadIO m => String -> m (Either String GetIssues)
fetchIssue searchText = fetch runReq (GetIssuesArgs searchText)
  where
    runReq :: MonadIO m => LBS.ByteString -> m LBS.ByteString
    runReq bs = do
      putTextLn $ "Sending this request: " <> decodeUtf8 bs
      pure "returned values"
```

The fetch method takes a callback to perform the request,
and the 'Args' version of the query (in this case GetIssuesArgs).

Running this helper will print the generated query based on the input.

# Run the query

In `Lentille.GitHub` there is a `runGithubGraphRequest` that can be used in place of performReq.
Let's modify fetchIssue to use that instead:

```haskell
fetchIssue :: MonadIO m => GitHubGraphClient -> String -> m (Either String GetIssues)
fetchIssue client searchText = fetch (runGithubGraphRequest client) (GetIssuesArgs searchText "")
```

To run this, you need to create a client first:

```ShellSesion
λ> client <- newGithubGraphClient "https://api.github.com/graphql"
λ> resp <- fetchIssue client "repo:morucci/reptest linked:pr updated:>=2021-05-02"
λ> :type resp
resp :: Either String GetIssues
-- Start the repl with `cabal repl --build-depends pretty-simple` to activate the pretty print:
λ> :set -interactive-print=Text.Pretty.Simple.pPrint
λ> Right
    ( GetIssues
        { search = SearchSearchResultItemConnection
            { nodes = Just
                [ Just
                    ( SearchNodesIssue
                        { title = "issue zulu"
                        , updatedAt = DateTime "2021-05-03T10:28:56Z"
                        , timelineItems = SearchNodesTimelineItemsIssueTimelineItemsConnection
                            { nodes = Just
                                [ Just
                                    ( SearchNodesTimelineItemsNodesConnectedEvent
                                        { subject = SearchNodesTimelineItemsNodesSubjectPullRequest
                                            { url = URI "https://github.com/morucci/reptest/pull/7" }
                                        }
                                    )
                                ]
                            }
                        }
                    )
       ...
```

# Create a streaming response

TODO: use `Lentille.GitHub.streamFetch`
