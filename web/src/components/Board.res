// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// The board view
//
open Prelude

let loadFromUrl = () =>
  URLSearchParams.current()
  ->URLSearchParams.get("q")
  ->Js.Nullable.toOption
  ->Belt.Option.getWithDefault("")

let saveToUrl = value => setLocationSearch("q", value)

@react.component
let make = (~index: string) => {
  let fields = useAutoGet(() => WebApi.Search.fields({version: "1"}))
  // The actual search bar content state
  let (searchText, setSearchText) = React.useState(loadFromUrl)
  // The search result from the api
  let (result, setResult) = React.useState(_ => None)
  let handleOk = (resp: WebApi.axiosResponse<SearchTypes.changes_query_response>) =>
    setResult(_ => resp.data->Some)->Js.Promise.resolve
  let onSearch = v => setSearchText(_ => v)
  React.useEffect1(() => {
    switch searchText {
    | "" => ignore()
    | query =>
      // searchText->saveToUrl
      ignore(WebApi.Search.changesQuery({index: index, query: query}) |> Js.Promise.then_(handleOk))
    }
    None
  }, [searchText])

  <MStack>
    <MStackItem>
      {switch fields {
      | Some(Ok({fields})) => <SearchBar initialValue={searchText} fields onSearch />
      | _ => React.null
      }}
    </MStackItem>
    <MStackItem>
      {switch result {
      | None => React.null
      | Some(SearchTypes.Error(err)) =>
        <Alert
          title={err.message ++ " at " ++ string_of_int(Int32.to_int(err.position))} variant=#Danger
        />
      | Some(SearchTypes.Items(items)) => {
          saveToUrl(searchText)
          let changes = items.changes->Belt.List.toArray
          switch changes->Belt.Array.length {
          | 0 => <p> {"No changes matched"->str} </p>
          | _ =>
            <Patternfly.DataList isCompact={true}>
              {changes->Belt.Array.map(change => <Change.Search change={change} />)->React.array}
            </Patternfly.DataList>
          }
        }
      }}
    </MStackItem>
  </MStack>
}

let default = make
