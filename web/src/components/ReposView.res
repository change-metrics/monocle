// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// The repositories summary view component
//

open Prelude

module RepoSummaryTable = {
  @react.component
  let make = (~repos: list<SearchTypes.repo_summary>) => {
    let (rows, setRows) = React.useState(_ => [])
    let (sortBy, setSortBy) = React.useState(_ => {index: 2, direction: #asc})

    let columns = [
      {title: "Repository", transforms: [sortable]},
      {title: "Total changes", transforms: [sortable]},
      {title: "Open changes", transforms: [sortable]},
      {title: "Merged changes", transforms: [sortable]},
      {title: "Abandoned changes", transforms: [sortable]},
    ]
    let isOrdered = (first: SearchTypes.repo_summary, second: SearchTypes.repo_summary, index) =>
      switch index {
      | 0 => first.fullname < second.fullname
      | 1 => first.total_changes < second.total_changes
      | 2 => first.open_changes < second.open_changes
      | 3 => first.merged_changes < second.merged_changes
      | 4 => first.abandoned_changes < second.abandoned_changes
      | _ => false
      }
    let formatters: list<SearchTypes.repo_summary => React.element> = list{
      repo => repo.fullname->str,
      repo => repo.total_changes->int32_str->str,
      repo => repo.open_changes->int32_str->str,
      repo => repo.merged_changes->int32_str->str,
      repo => repo.abandoned_changes->int32_str->str,
    }

    let doSort = rows => rows->sortRows(isOrdered)
    let onSort = (_, index, direction) => {
      setRows(_ => doSort(rows, index, direction))
      setSortBy(_ => {index: index, direction: direction})
    }

    React.useEffect1(() => {
      setRows(_ => repos->mkRows(formatters)->doSort(sortBy.index, sortBy.direction))
      None
    }, [repos])
    <Table caption="Repository summary" rows cells=columns sortBy onSort>
      <TableHeader /> <TableBody />
    </Table>
  }
}

@react.component
let make = (~store: Store.t) => {
  let (state, _) = store
  let index = state.index
  let query = addQuery(state.query, state.filter)
  let request = {
    SearchTypes.index: index,
    query: query,
    username: "",
    query_type: SearchTypes.Query_repos_summary,
    // order and limit are not handled server side
    order: None,
    limit: 0->Int32.of_int,
  }
  Js.log2("HERE", query)
  <div>
    {switch useAutoGetOn(() => WebApi.Search.query(request), query) {
    | None => <Spinner />
    | Some(Error(title)) => <Alert variant=#Danger title />
    | Some(Ok(SearchTypes.Error(err))) =>
      <Alert
        title={err.message ++ " at " ++ string_of_int(Int32.to_int(err.position))} variant=#Danger
      />
    | Some(Ok(SearchTypes.Repos_summary(repos))) =>
      let reposum = repos.reposum
      switch reposum->Belt.List.length {
      | 0 => <p> {"No repository matched"->str} </p>
      | n => {
          Js.log("Render: " ++ n->string_of_int)
          <MCenteredContent>
            <Card isCompact=true>
              <CardTitle> {"Repository summary"->str} </CardTitle>
              <CardBody> <RepoSummaryTable repos=reposum /> </CardBody>
            </Card>
          </MCenteredContent>
        }
      }
    | Some(Ok(_)) => React.null
    }}
  </div>
}
let default = make
