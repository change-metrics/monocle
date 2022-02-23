// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// The repositories summary view component
//

open Prelude
open MLink

module ChangeLink = {
  type t =
    | AllChanges(string)
    | OpenChanges(string)
    | ScopedOpenChanges(string)
    | MergedChanges(string)
    | ScopedMergedChanges(string)
    | AbandonedChanges(string)

  let getFilter = (name: string, cStateM: option<string>) =>
    "repo:" ++
    name ++
    " " ++
    switch cStateM {
    | Some(cState) => "state:" ++ cState
    | None => ""
    }

  let createFilter = (entity: t): (string, option<Store.Store.action>) =>
    switch entity {
    | AllChanges(n) => (getFilter(n, None), None)
    | OpenChanges(n) => (getFilter(n, "open"->Some), None)
    | ScopedOpenChanges(n) => ("repo:" ++ n, SetAuthorScopedTab(OpenChanges)->Some)
    | MergedChanges(n) => (getFilter(n, "merged"->Some), None)
    | ScopedMergedChanges(n) => ("repo:" ++ n, SetAuthorScopedTab(MergedChanges)->Some)
    | AbandonedChanges(n) => (getFilter(n, "abandoned"->Some), None)
    }

  @react.component
  let make = (~store: Store.t, ~entity: t, ~path: string, ~name: string) => {
    let (filter, action) = createFilter(entity)
    <MonoLink store filter path name ?action />
  }
}

module RepoSummaryTable = {
  @react.component
  let make = (~store: Store.t, ~repos: list<SearchTypes.repo_summary>) => {
    let columnNames = [
      "Repository",
      "Open changes",
      "Created changes",
      "Updated changes",
      "Merged changes",
      "Abandoned changes",
    ]

    let getPath = (): (string, bool) =>
      switch readWindowLocationPathname()->Js.String2.split("/") {
      | ["", _, "group", name] => ("group" ++ "/" ++ name, true)
      | ["", _, "author", name] => ("author" ++ "/" ++ name, true)
      | _ => ("changes", false)
      }

    let isOrdered = (first: SearchTypes.repo_summary, second: SearchTypes.repo_summary, index) =>
      switch index {
      | 0 => first.fullname < second.fullname
      | 1 => first.open_changes < second.open_changes
      | 2 => first.created_changes < second.created_changes
      | 3 => first.updated_changes < second.updated_changes
      | 4 => first.merged_changes < second.merged_changes
      | 5 => first.abandoned_changes < second.abandoned_changes
      | _ => false
      }

    let (path, isScoped) = getPath()
    let mkLink = (entity: ChangeLink.t, label: string) =>
      <ChangeLink store entity path name={label} />
    let formatters: list<SearchTypes.repo_summary => React.element> = list{
      repo => repo.fullname->str,
      repo =>
        (
          isScoped
            ? ChangeLink.ScopedOpenChanges(repo.fullname)
            : ChangeLink.OpenChanges(repo.fullname)
        )->mkLink(repo.open_changes->int32_str),
      repo => repo.created_changes->int32_str->str,
      repo => repo.updated_changes->int32_str->str,
      repo =>
        (
          isScoped
            ? ChangeLink.ScopedMergedChanges(repo.fullname)
            : ChangeLink.MergedChanges(repo.fullname)
        )->mkLink(repo.merged_changes->int32_str),
      repo =>
        isScoped
          ? repo.abandoned_changes->int32_str->str
          : ChangeLink.AbandonedChanges(repo.fullname)->mkLink(repo.abandoned_changes->int32_str),
    }

    <SortableTable items=repos defaultSortedColumn=2 columnNames isOrdered formatters />
  }
}

@react.component
let make = (~store: Store.t, ~extraQuery: option<string>=?) => {
  let (state, _) = store
  let baseRequest = Store.mkSearchRequest(state, SearchTypes.Query_repos_summary)
  let request = {
    ...baseRequest,
    query: switch extraQuery {
    | Some(ex) => addQuery(baseRequest.query, ex)
    | None => baseRequest.query
    },
  }
  let trigger = state.query ++ extraQuery->Belt.Option.getWithDefault("")
  let tooltip_content = "This shows the list of repositories (which received some activities) along with some metrics"
  let title = "Repository summary"
  let icon = <Patternfly.Icons.Repository />
  let match = resp =>
    switch resp {
    | SearchTypes.Repos_summary(data) => Some(data.reposum)
    | _ => None
    }
  let childrenBuilder = (repos: list<Web.SearchTypes.repo_summary>) =>
    <RepoSummaryTable store repos />

  <QueryRenderCard request trigger title tooltip_content icon match childrenBuilder />
}
let default = make
