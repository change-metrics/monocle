// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// The peers strength view component
//

open Prelude

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

@react.component
let make = (~store: Store.t) => {
  let (state, _) = store
  let index = state.index
  let query = state.query
  let request = {
    SearchTypes.index: index,
    query: query,
    username: "",
    query_type: SearchTypes.Query_top_authors_peers,
    // Not hendled server side
    order: None,
    limit: 0->Int32.of_int,
  }
  <div>
    {switch useAutoGetOn(() => WebApi.Search.query(request), query) {
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
              // <MGridItem> <LimitSelector limit setLimit /> </MGridItem>
            </MGrid>
          </CardTitle>
          <CardBody> <PeersStrengthTable items={tps.author_peer} /> </CardBody>
        </Card>
      </MCenteredContent>
    | Some(Ok(_)) => React.null
    }}
  </div>
}

let default = make
