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
    | ScopedAbandonedChanges(string)

  let getFilter = (name: string, cStateM: option<string>) =>
    "repo:" ++
    name ++
    " " ++
    switch cStateM {
    | Some(cState) => "state:" ++ cState
    | None => ""
    }

  let createFilter = (entity: t): (string, option<Store.Store.action>) => {
    let toScoped = (n, tabType) => ("repo:" ++ n, Store.Store.SetAuthorScopedTab(tabType)->Some)
    switch entity {
    | AllChanges(n) => (getFilter(n, None), None)
    | OpenChanges(n) => (getFilter(n, "open"->Some), None)
    | ScopedOpenChanges(n) => n->toScoped(OpenChanges)
    | ScopedMergedChanges(n) => n->toScoped(MergedChanges)
    | ScopedAbandonedChanges(n) => n->toScoped(AbandonedChanges)
    | MergedChanges(n) => (getFilter(n, "merged"->Some), None)
    | AbandonedChanges(n) => (getFilter(n, "abandoned"->Some), None)
    }
  }

  @react.component
  let make = (~store: Store.t, ~entity: t, ~path: option<string>=?, ~name: string) => {
    let (filter, action) = createFilter(entity)
    <MonoLink store filter ?path name ?action />
  }
}

module RepoSummaryTable = {
  @react.component
  let make = (
    ~store: Store.t,
    ~repos: list<SearchTypes.repo_summary>,
    ~isScoped: option<bool>=?,
  ) => {
    let columnNames = [
      "Repository",
      "Open changes",
      "Created changes",
      "Updated changes",
      "Merged changes",
      "Abandoned changes",
    ]

    let scoped = switch isScoped {
    | Some(true) => true
    | _ => false
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

    let mkLink = (entity: ChangeLink.t, label: string) =>
      scoped
        ? <ChangeLink store entity name={label} />
        : <ChangeLink store entity path="changes" name={label} />

    let formatters: list<SearchTypes.repo_summary => React.element> = list{
      repo => repo.fullname->str,
      repo =>
        (
          scoped
            ? ChangeLink.ScopedOpenChanges(repo.fullname)
            : ChangeLink.OpenChanges(repo.fullname)
        )->mkLink(repo.open_changes->int32_str),
      repo => repo.created_changes->int32_str->str,
      repo => repo.updated_changes->int32_str->str,
      repo =>
        (
          scoped
            ? ChangeLink.ScopedMergedChanges(repo.fullname)
            : ChangeLink.MergedChanges(repo.fullname)
        )->mkLink(repo.merged_changes->int32_str),
      repo =>
        (
          scoped
            ? ChangeLink.ScopedAbandonedChanges(repo.fullname)
            : ChangeLink.AbandonedChanges(repo.fullname)
        )->mkLink(repo.abandoned_changes->int32_str),
    }

    <SortableTable items=repos defaultSortedColumn=2 columnNames isOrdered formatters />
  }
}

@react.component
let make = (~store: Store.t, ~extraQuery: option<string>=?, ~isScoped: option<bool>=?) => {
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
    <RepoSummaryTable store repos ?isScoped />

  <QueryRenderCard request trigger title tooltip_content icon match childrenBuilder />
}
let default = make
