// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// The peers strength view component
//

open Prelude

module ConnectionDiagram = {
  type t = {a1: string, a2: string, s: int}
  @react.component @module("./connection_diagram")
  external make: (~data: array<t>) => React.element = "default"

  let adapt = (xs: list<SearchTypes.author_peer>) =>
    xs
    ->Belt.List.map(x => {a1: x.author, a2: x.peer, s: x.strength->Int32.to_int})
    ->Belt.List.toArray
}

module PeersStrengthTable = {
  @react.component
  let make = (~items: list<SearchTypes.author_peer>) => {
    let columnNames = ["Change author", "Peer reviewer", "Strength"]

    let isOrdered = (first: SearchTypes.author_peer, second: SearchTypes.author_peer, index) =>
      switch index {
      | 0 => first.author < second.author
      | 1 => first.peer < second.peer
      | 2 => first.strength < second.strength
      | _ => false
      }
    let formatters: list<SearchTypes.author_peer => React.element> = list{
      item => item.author->str,
      item => item.peer->str,
      item => item.strength->int32_str->str,
    }

    <SortableTable items defaultSortedColumn=2 columnNames isOrdered formatters />
  }
}

module LimitSelector = {
  @react.component
  let make = (~limit: int, ~setLimit: (int => int) => unit) => {
    let setLimit' = str => {
      let v = str == "" ? 25 : str->int_of_string
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

@react.component
let make = (~store: Store.t) => {
  let (state, _) = store
  let index = state.index
  let query = state.query
  let (limit, setLimit) = React.useState(() => 25)
  let request = {
    SearchTypes.index: index,
    query: query,
    username: "",
    query_type: SearchTypes.Query_top_authors_peers,
    // Not hendled server side
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
    | Some(Ok(SearchTypes.Authors_peers(tps))) =>
      <MCenteredContent>
        <Card isCompact=true>
          <CardTitle>
            <MGrid>
              <MGridItem> {"Peers Strength"->str} </MGridItem>
              <MGridItem> <LimitSelector limit setLimit /> </MGridItem>
            </MGrid>
          </CardTitle>
          <CardBody>
            <ConnectionDiagram data={tps.author_peer->ConnectionDiagram.adapt} />
            <PeersStrengthTable items={tps.author_peer} />
          </CardBody>
        </Card>
      </MCenteredContent>
    | Some(Ok(_)) => React.null
    }}
  </div>
}

let default = make
