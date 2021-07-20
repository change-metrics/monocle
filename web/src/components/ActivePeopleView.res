// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// The most active authors view component
//

open Prelude

module TopTermsTable = {
  @react.component
  let make = (~terms: list<SearchTypes.term_count>) => {
    let columnNames = ["Name", "Count"]

    let isOrdered = (first: SearchTypes.term_count, second: SearchTypes.term_count, index) =>
      switch index {
      | 0 => first.term < second.term
      | 1 => first.count < second.count
      | _ => false
      }
    let formatters: list<SearchTypes.term_count => React.element> = list{
      item => item.term->str,
      item => item.count->int32_str->str,
    }

    <SortableTable items=terms defaultSortedColumn=1 columnNames isOrdered formatters />
  }
}

module LimitSelector = {
  @react.component
  let make = (~limit: int, ~setLimit: (int => int) => unit) => {
    let setLimit' = str => {
      let v = str == "" ? 10 : str->int_of_string
      setLimit(_ => v)
    }
    <Patternfly.Layout.Bullseye>
      <MSelect
        placeholder={"Set limit"}
        options={list{10, 25, 50, 100, 500}->Belt.List.map(string_of_int)}
        multi={false}
        value={limit > 0 ? limit->string_of_int : ""}
        valueChanged={setLimit'}
      />
    </Patternfly.Layout.Bullseye>
  }
}

module MostActiveAuthor = {
  @react.component
  let make = (~store: Store.t, ~qtype: SearchTypes.query_request_query_type, ~title: string) => {
    let (state, _) = store
    let index = state.index
    let query = state.query
    let (limit, setLimit) = React.useState(() => 10)
    let request = {
      SearchTypes.index: index,
      query: query,
      username: "",
      query_type: qtype,
      order: None,
      limit: limit->Int32.of_int,
    }
    <div>
      {switch useAutoGetOn(() => WebApi.Search.query(request), query ++ limit->string_of_int) {
      | None => <Spinner />
      | Some(Error(title)) => <Alert variant=#Danger title />
      | Some(Ok(SearchTypes.Error(err))) =>
        <Alert
          title={err.message ++ " at " ++ string_of_int(Int32.to_int(err.position))} variant=#Danger
        />
      | Some(Ok(SearchTypes.Top_authors(tsc))) =>
        <MCenteredContent>
          <Card isCompact=true>
            <CardTitle>
              <MGrid>
                <MGridItem> {title} </MGridItem>
                <MGridItem> <LimitSelector limit setLimit /> </MGridItem>
              </MGrid>
            </CardTitle>
            <CardBody> <TopTermsTable terms=tsc.termcount /> </CardBody>
          </Card>
        </MCenteredContent>
      | Some(Ok(_)) => React.null
      }}
    </div>
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

  let getQD = (qt: t): (SearchTypes.query_request_query_type, string) =>
    switch qt {
    | ByChangeCreated => (SearchTypes.Query_top_authors_changes_created, "By changes created")
    | ByChangeMerged => (SearchTypes.Query_top_authors_changes_merged, "By changes merged")
    | ByChangeReviewed => (SearchTypes.Query_top_authors_changes_reviewed, "By reviews")
    | ByChangeCommented => (SearchTypes.Query_top_authors_changes_commented, "By comments")
    | ByMostReviewed => (SearchTypes.Query_top_reviewed_authors, "By changes reviewed")
    | ByMostCommented => (SearchTypes.Query_top_commented_authors, "By changes commented")
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
    let (qtype, title) = item->TopMetricsInfo.getQD
    <MStackItem key=title> <MostActiveAuthor store qtype title /> </MStackItem>
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
