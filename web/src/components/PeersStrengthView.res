// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// The peers strength view component
//

open Prelude

module ConnectionDiagram = {
  type t = {a1: string, a2: string, s: int}
  @react.component @module("./chartjs.jsx")
  external make: (~data: array<t>) => React.element = "ConnectionDiagram"

  let adapt = (peers: list<SearchTypes.author_peer>) => {
    let rec go = (xs: list<SearchTypes.author_peer>, acc: list<t>) =>
      switch xs {
      | list{} => acc
      | list{x, ...rest} => {
          // Look for a reverse peer (and remove it from the remaining list)
          let (otherLink, remaining) =
            rest->Belt.List.partition(e => x.author == e.peer && x.peer == e.author)
          // If there is one, get its strength
          let extraStrength = switch otherLink {
          // here we assume there can only be one other link
          | list{y} => y.strength->Int32.to_int
          | _ => 0
          }
          // Continue with the remaining
          remaining->go(
            acc->Belt.List.add({
              a1: x.author,
              a2: x.peer,
              s: x.strength->Int32.to_int + extraStrength,
            }),
          )
        }
      }
    peers->go(list{})->Belt.List.toArray
  }
}

module PeersStrengthTable = {
  @react.component
  let make = (~items: list<SearchTypes.author_peer>) => {
    let columnNames = ["Peer reviewer", "Change author", "Strength"]

    let isOrdered = (first: SearchTypes.author_peer, second: SearchTypes.author_peer, index) =>
      switch index {
      | 0 => first.peer < second.peer
      | 1 => first.author < second.author
      | 2 => first.strength < second.strength
      | _ => false
      }
    let formatters: list<SearchTypes.author_peer => React.element> = list{
      item => item.peer->str,
      item => item.author->str,
      item => item.strength->int32_str->str,
    }

    <SortableTable items defaultSortedColumn=2 columnNames isOrdered formatters />
  }
}

@react.component
let make = (~store: Store.t, ~stacked: bool, ~extraQuery: option<string>=?) => {
  let (state, _) = store
  let (limit, setLimit) = React.useState(() => 25)
  let limit_values = list{10, 25, 50, 100, 500}
  let tooltip_content = "This shows the strength between change authors and peer reviewers"
  let baseRequest = Store.mkSearchRequest(state, SearchTypes.Query_top_authors_peers)
  let request = {
    ...baseRequest,
    query: switch extraQuery {
    | Some(ex) => addQuery(baseRequest.query, ex)
    | None => baseRequest.query
    },
    limit: limit->Int32.of_int,
  }
  let trigger = state.query ++ limit->string_of_int
  let limitSelector = <LimitSelector limit setLimit default=25 values=limit_values />
  let title = "Peers Strength"
  let icon = <Patternfly.Icons.Integration />
  let match = resp =>
    switch resp {
    | SearchTypes.Authors_peers(tps) => Some(tps)
    | _ => None
    }
  let childrenBuilder = (data: Web.SearchTypes.authors_peers) =>
    stacked
      ? <MStack>
          <MStackItem>
            <ConnectionDiagram data={data.author_peer->ConnectionDiagram.adapt} />
          </MStackItem>
          <MStackItem> <PeersStrengthTable items={data.author_peer} /> </MStackItem>
        </MStack>
      : <MGrid>
          <MGridItemXl5> <PeersStrengthTable items={data.author_peer} /> </MGridItemXl5>
          <MGridItemXl7>
            <ConnectionDiagram data={data.author_peer->ConnectionDiagram.adapt} />
          </MGridItemXl7>
        </MGrid>

  <QueryRenderCard request trigger title tooltip_content icon limitSelector match childrenBuilder />
}

let default = make
