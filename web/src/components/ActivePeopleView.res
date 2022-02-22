// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// The most active authors view component
//

open Prelude

module TopTermsTable = {
  type t = NoLink | AuthorLink | ScopedAuthorLink | AuthorWithFilter(string)
  @react.component
  let make = (
    ~store,
    ~items: list<SearchTypes.term_count>,
    ~columnNames: array<string>,
    ~link: t,
  ) => {
    let (state, dispatch) = store
    let isOrdered = (first: SearchTypes.term_count, second: SearchTypes.term_count, index) =>
      switch index {
      | 0 => first.term < second.term
      | 1 => first.count < second.count
      | _ => false
      }
    let mkFilter = (author, filter) =>
      "author:\"" ++
      author ++
      "\"" ++
      switch filter {
      | None => ""
      | Some(extra) => " " ++ extra
      }
    let mkAuthorLink = (name, extraFilter) =>
      <MLink.MonoLink store filter={name->mkFilter(extraFilter)} path={"changes"} name />
    let formatters: list<SearchTypes.term_count => React.element> = list{
      item =>
        switch link {
        | NoLink => item.term->str
        | AuthorLink => item.term->mkAuthorLink(None)
        | AuthorWithFilter(filter) => item.term->mkAuthorLink(filter->Some)
        | ScopedAuthorLink =>
          <a
            onClick={e => {
              let link = "/" ++ state.index ++ "/author/" ++ item.term->Js.Global.encodeURIComponent
              e->ReactEvent.Mouse.preventDefault
              "1"->SetAuthorScopedTab->dispatch
              link->RescriptReactRouter.push
            }}>
            {item.term->str}
          </a>
        },
      item => item.count->int32_str->str,
    }

    <SortableTable items defaultSortedColumn=1 columnNames isOrdered formatters />
  }
}

module MostActiveAuthor = {
  @react.component
  let make = (
    ~store: Store.t,
    ~qtype: SearchTypes.query_request_query_type,
    ~title: string,
    ~tooltip_content: string,
    ~link: TopTermsTable.t,
    ~extraQuery: option<string>=?,
  ) => {
    let (state, _) = store
    let (limit, setLimit) = React.useState(() => 10)
    let limit_values = list{10, 25, 50, 100, 500}
    let columnNames = ["Name", "Count"]
    let baseRequest = Store.mkSearchRequest(state, qtype)
    let request = {
      ...baseRequest,
      query: switch extraQuery {
      | None => baseRequest.query
      | Some(ex) => addQuery(baseRequest.query, ex)
      },
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
      <TopTermsTable store items=data.termcount columnNames link />
    <QueryRenderCard
      request trigger title tooltip_content icon limitSelector match childrenBuilder
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

  let getQD = (qt: t): (SearchTypes.query_request_query_type, string, string, TopTermsTable.t) => {
    let tooltip_prefix = "This shows a list of change' authors ordered by the amount of "
    switch qt {
    | ByChangeCreated => (
        SearchTypes.Query_top_authors_changes_created,
        "By changes created",
        tooltip_prefix ++ "changes they created",
        TopTermsTable.AuthorLink,
      )
    | ByChangeMerged => (
        SearchTypes.Query_top_authors_changes_merged,
        "By changes merged",
        tooltip_prefix ++ "changes they merged",
        TopTermsTable.AuthorWithFilter("state:merged"),
      )
    | ByChangeReviewed => (
        SearchTypes.Query_top_authors_changes_reviewed,
        "By reviews",
        tooltip_prefix ++ "reviews they performed",
        TopTermsTable.NoLink,
      )
    | ByChangeCommented => (
        SearchTypes.Query_top_authors_changes_commented,
        "By comments",
        tooltip_prefix ++ "comments they wrote",
        TopTermsTable.NoLink,
      )
    | ByMostReviewed => (
        SearchTypes.Query_top_reviewed_authors,
        "By changes reviewed",
        tooltip_prefix ++ "reviews they got",
        TopTermsTable.NoLink,
      )
    | ByMostCommented => (
        SearchTypes.Query_top_commented_authors,
        "By changes commented",
        tooltip_prefix ++ "comments they received",
        TopTermsTable.NoLink,
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
    let (qtype, title, tooltip_content, link) = item->TopMetricsInfo.getQD
    <MostActiveAuthor store qtype title tooltip_content link />
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
