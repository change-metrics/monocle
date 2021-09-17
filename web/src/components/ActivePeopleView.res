// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// The most active authors view component
//

open Prelude

module MostActiveAuthor = {
  @react.component
  let make = (
    ~store: Store.t,
    ~qtype: SearchTypes.query_request_query_type,
    ~title: string,
    ~tooltip_content: string,
  ) => {
    let (state, _) = store
    let (limit, setLimit) = React.useState(() => 10)
    let limit_values = list{10, 25, 50, 100, 500}
    let columnNames = ["Name", "Count"]
    let request = {
      ...Store.mkSearchRequest(state, qtype),
      limit: limit->Int32.of_int,
    }
    let trigger = state.query ++ limit->string_of_int
    let limitSelector = <LimitSelector limit setLimit default=10 values=limit_values />
    let icon = <Patternfly.Icons.TrendUp />
    let match = resp =>
      switch resp {
      | SearchTypes.Top_authors(data) => Some(data)
      | _ => None
      }
    let childrenBuilder = (data: Web.SearchTypes.terms_count) =>
      <TopTermsTable items=data.termcount columnNames />
    <QueryRenderCard
      request
      trigger
      title
      tooltip_content
      icon
      limitSelector
      match
      childrenBuilder
      isCentered=false
    />
  }
}

module TopMetricsInfo = {
  type t =
    | ByChangeCreated
    | ByChangeMerged
    | ByChangeReviewed
    | ByChangeCommented
    | ByMostReviewed
    | ByMostCommented

  let getQD = (qt: t): (SearchTypes.query_request_query_type, string, string) => {
    let tooltip_prefix = "This shows a list of change' authors ordered by the amount of "
    switch qt {
    | ByChangeCreated => (
        SearchTypes.Query_top_authors_changes_created,
        "By changes created",
        tooltip_prefix ++ "changes they created",
      )
    | ByChangeMerged => (
        SearchTypes.Query_top_authors_changes_merged,
        "By changes merged",
        tooltip_prefix ++ "changes they merged",
      )
    | ByChangeReviewed => (
        SearchTypes.Query_top_authors_changes_reviewed,
        "By reviews",
        tooltip_prefix ++ "reviews they performed",
      )
    | ByChangeCommented => (
        SearchTypes.Query_top_authors_changes_commented,
        "By comments",
        tooltip_prefix ++ "comments they wrote",
      )
    | ByMostReviewed => (
        SearchTypes.Query_top_reviewed_authors,
        "By changes reviewed",
        tooltip_prefix ++ "reviews they got",
      )
    | ByMostCommented => (
        SearchTypes.Query_top_commented_authors,
        "By changes commented",
        tooltip_prefix ++ "comments they received",
      )
    }
  }
}

@react.component
let make = (~store: Store.t) => {
  open TopMetricsInfo
  let layout = [
    (ByChangeCreated, ByChangeMerged),
    (ByChangeReviewed, ByChangeCommented),
    (ByMostReviewed, ByMostCommented),
  ]
  let getItem = (item: t) => {
    let (qtype, title, tooltip_content) = item->TopMetricsInfo.getQD
    <MostActiveAuthor store qtype title tooltip_content />
  }
  let getItemL = ((l, _)) => l->getItem
  let getItemR = ((_, r)) => r->getItem
  <MCenteredContent>
    <MStack>
      {layout->Belt.Array.mapWithIndex((i, e) => {
        <MStackItem key={i->string_of_int}>
          <MGrid>
            <MGridItemXl6> {e->getItemL} </MGridItemXl6>
            <MGridItemXl6> {e->getItemR} </MGridItemXl6>
          </MGrid>
        </MStackItem>
      })}
    </MStack>
  </MCenteredContent>
}

let default = make
