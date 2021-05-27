// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// The board view
//
open Prelude

@react.component
let make = (~index: string) => {
  // The actual search bar content state
  let (searchText, setSearchText) = React.useState(_ => "")
  // The search result from the api
  let (result, setResult) = React.useState(_ => None)
  let handleOk = (resp: WebApi.axiosResponse<SearchTypes.changes_query_response>) =>
    setResult(_ => resp.data->Some)->Js.Promise.resolve
  let onSearch = queryM =>
    switch queryM {
    | "" => ignore()
    | query =>
      ignore(WebApi.Search.changesQuery({index: index, query: query}) |> Js.Promise.then_(handleOk))
    }
  let renderChange = (change: SearchTypes.change) =>
    <Prelude.DataListItemRow key={change.url}>
      <Prelude.DataListCell> <span> {change.title->str} </span> </Prelude.DataListCell>
    </Prelude.DataListItemRow>

  <MStack>
    <MStackItem>
      <SearchBar value={searchText} onChange={(v, _) => setSearchText(_ => v)} onSearch />
    </MStackItem>
    <MStackItem>
      {switch result {
      | None => React.null
      | Some(SearchTypes.Error(err)) =>
        <Alert title={err.message ++ " at " ++ string_of_int(Int32.to_int(err.position))} />
      | Some(SearchTypes.Items(items)) =>
        <Patternfly.DataList isCompact={true}>
          {items.changes->Belt.List.map(renderChange)->Belt.List.toArray->React.array}
        </Patternfly.DataList>
      }}
    </MStackItem>
  </MStack>
}

let default = make
