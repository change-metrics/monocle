// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// The repositories summary view component
//

open Prelude

module RepoSummaryTable = {
  @react.component
  let make = (~repos: list<SearchTypes.repo_summary>) => {
    let columns = [
      {title: "Repository", transforms: [sortable]},
      {title: "Total changes", transforms: [sortable]},
      {title: "Open changes", transforms: [sortable]},
      {title: "Merged changes", transforms: [sortable]},
      {title: "Abandoned changes", transforms: [sortable]},
    ]
    let makeCells = (repo: SearchTypes.repo_summary) => {
      {
        cells: [
          repo.fullname,
          repo.total_changes->int32_str,
          repo.open_changes->int32_str,
          repo.merged_changes->int32_str,
          repo.abandoned_changes->int32_str,
        ],
      }
    }
    let (rows, setRows) = React.useState(_ => repos->Belt.List.map(makeCells)->Belt.List.toArray)
    let onSort = _ => {
      setRows(_ => Belt.Array.reverse(rows))
    }

    <Table caption="Repository summary" rows cells=columns onSort>
      <TableHeader /> <TableBody />
    </Table>
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
      let reposum = repos.reposum
      switch reposum->Belt.List.length {
      | 0 => <p> {"No repository matched"->str} </p>
      | _ => <RepoSummaryTable repos=reposum />
      }
    }}
  </div>
}
let default = make
