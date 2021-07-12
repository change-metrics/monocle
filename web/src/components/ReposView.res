// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// The user group view component
//

open Prelude

@react.component
let make = (~store: Store.t) => {
  let (state, _) = store
  let request = {
    SearchTypes.index: state.index,
    query: state.query,
    username: "",
    query_type: SearchTypes.Query_repos_summary,
  }
  <div>
    {switch useAutoGetOn(() => WebApi.Search.query(request), state.query) {
    | None => <Spinner />
    | Some(Error(title)) => <Alert variant=#Danger title />
    | Some(Ok(SearchTypes.Error(err))) =>
      <Alert
        title={err.message ++ " at " ++ string_of_int(Int32.to_int(err.position))} variant=#Danger
      />
    | Some(Ok(SearchTypes.Changes(_))) => React.null
    | Some(Ok(SearchTypes.Repos_summary(repos))) => {
        Js.log(repos)
        React.null
      }
    }}
  </div>
}
let default = make
