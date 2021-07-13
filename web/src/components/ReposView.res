// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// The repositories summary view component
//

open Prelude

module RowItem = {
  module Head = {
    @react.component
    let make = () =>
      <thead>
        <tr role="row">
          <th role="columnheader"> {"Repository"->str} </th>
          <th role="columnheader"> {"Total changes"->str} </th>
          <th role="columnheader"> {"Open changes"->str} </th>
          <th role="columnheader"> {"Merged changes"->str} </th>
          <th role="columnheader"> {"Abandoned changes"->str} </th>
        </tr>
      </thead>
  }
  @react.component
  let make = (~repo: SearchTypes.repo_summary) => {
    <tr role="row">
      <td role="cell"> {repo.fullname->str} </td>
      <td role="cell"> {repo.total_changes->int32_str} </td>
      <td role="cell"> {repo.open_changes->int32_str} </td>
      <td role="cell"> {repo.merged_changes->int32_str} </td>
      <td role="cell"> {repo.abandoned_changes->int32_str} </td>
    </tr>
  }
}

module ReposSumTable = {
  @react.component
  let make = (~repos: SearchTypes.repos_summary) => {
    <table className="pf-c-table pf-m-compact pf-m-grid-md" role="grid">
      <RowItem.Head />
      <tbody role="rowgroup">
        {repos.reposum
        ->Belt.List.mapWithIndex((idx, repo) => <RowItem key={string_of_int(idx)} repo />)
        ->Belt.List.toArray
        ->React.array}
      </tbody>
    </table>
  }
}

@react.component
let make = (~store: Store.t) => {
  let (state, _) = store
  let request = {
    SearchTypes.index: state.index,
    query: state.query,
    username: "",
    query_type: SearchTypes.Query_repos_summary,
    order: None,
    limit: 100->Int32.of_int,
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
    | Some(Ok(SearchTypes.Repos_summary(repos))) =>
      switch repos.reposum->Belt.List.length {
      | 0 => <p> {"No repository matched"->str} </p>
      | _ => <ReposSumTable repos />
      }
    }}
  </div>
}
let default = make
