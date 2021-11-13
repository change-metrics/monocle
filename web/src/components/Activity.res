// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// The activity view
//
open Prelude
open MLink

module GraphWithStats = {
  @react.component
  let make = (~graph: React.element, ~stats: list<React.element>) => {
    <Patternfly.Layout.Stack hasGutter={true}>
      <Patternfly.Layout.StackItem>
        <Card> <CardBody> {graph} </CardBody> </Card>
      </Patternfly.Layout.StackItem>
      <Patternfly.Layout.Grid hasGutter={false}>
        {stats
        ->Belt.List.mapWithIndex((i, statE) =>
          <Patternfly.Layout.GridItem key={i->string_of_int} md=Column._6 xl=Column._4>
            <Card isCompact={true}> <CardBody> {statE} </CardBody> </Card>
          </Patternfly.Layout.GridItem>
        )
        ->Belt.List.toArray
        ->React.array}
      </Patternfly.Layout.Grid>
    </Patternfly.Layout.Stack>
  }
}

module ChangesLifeCycleStats = {
  module ChangesLifeCycleHisto = {
    @react.component @module("./changes_lifecycle.jsx")
    external make: (
      ~created: array<SearchTypes.histo>,
      ~updated: array<SearchTypes.histo>,
      ~merged: array<SearchTypes.histo>,
      ~abandoned: array<SearchTypes.histo>,
    ) => React.element = "CChangesLifeCycleHisto"
  }
  @react.component
  let make = (~store: Store.t) => {
    let (state, _) = store
    let request = Store.mkSearchRequest(state, SearchTypes.Query_changes_lifecycle_stats)
    let trigger = state.query
    let title = "Changes lifecycle stats"
    let tooltip_content = "This shows trends of change related metrics such as the evolution of the amount of change created"
    let icon = <Patternfly.Icons.Running />

    let match = resp =>
      switch resp {
      | SearchTypes.Lifecycle_stats(data) => Some(data)
      | _ => None
      }
    let childrenBuilder = (data: Web.SearchTypes.lifecycle_stats) => {
      let graph =
        <ChangesLifeCycleHisto
          created={data.created_histo->Belt.List.toArray}
          updated={data.updated_histo->Belt.List.toArray}
          merged={data.merged_histo->Belt.List.toArray}
          abandoned={data.abandoned_histo->Belt.List.toArray}
        />
      let stats = list{
        {
          switch data.created {
          | Some(created) =>
            (created.events_count->int32_str ++
            " changes created by " ++
            created.authors_count->int32_str ++ " authors")->str
          | None => React.null
          }
        },
        <MonoLink
          store
          filter="state:abandoned"
          path="changes"
          name={data.abandoned->int32_str ++ " changes abandoned"}
        />,
        <MonoLink
          store
          filter="state:merged"
          path="changes"
          name={data.merged->int32_str ++ " changes merged"}
        />,
        <MonoLink
          store
          filter="state:self_merged"
          path="changes"
          name={data.self_merged->int32_str ++ " changes self merged"}
        />,
        ("Mean Time To Merge: " ++ data.ttm_mean->momentHumanizeDuration)->str,
        ("TTM Median Deviation: " ++ data.ttm_variability->momentHumanizeDuration)->str,
        (data.updates_of_changes->int32_str ++ " updates of changes")->str,
        ("Changes with tests: " ++ data.changes_with_tests->Belt.Float.toString ++ "%")->str,
        (data.iterations_per_change->Belt.Float.toString ++ " iterations per change")->str,
        (data.commits_per_change->Belt.Float.toString ++ " commits per change")->str,
      }
      <GraphWithStats graph stats />
    }
    <QueryRenderCard
      request trigger title tooltip_content icon match childrenBuilder isCentered=false
    />
  }
}

module ChangesReviewStats = {
  module CChangesReviewHisto = {
    @react.component @module("./changes_review.jsx")
    external make: (
      ~comment_histo: array<SearchTypes.histo>,
      ~review_histo: array<SearchTypes.histo>,
    ) => React.element = "CChangeReviewEventsHisto"
  }
  @react.component
  let make = (~store: Store.t) => {
    let (state, _) = store
    let request = Store.mkSearchRequest(state, SearchTypes.Query_changes_review_stats)
    let trigger = state.query
    let title = "Changes review stats"
    let tooltip_content = "This shows trends of reviews and comments"
    let icon = <Patternfly.Icons.OutlinedComments />

    let match = resp =>
      switch resp {
      | SearchTypes.Review_stats(data) => Some(data)
      | _ => None
      }
    let childrenBuilder = (data: Web.SearchTypes.review_stats) => {
      let graph =
        <CChangesReviewHisto
          comment_histo={data.comment_histo->Belt.List.toArray}
          review_histo={data.review_histo->Belt.List.toArray}
        />
      let stats = list{
        switch data.comment_count {
        | Some(comment) =>
          (comment.events_count->int32_str ++
          " changes commented by " ++
          comment.authors_count->int32_str ++ " authors")->str

        | None => React.null
        },
        switch data.review_count {
        | Some(review) =>
          (review.events_count->int32_str ++
          " changes reviewed by " ++
          review.authors_count->int32_str ++ " authors")->str

        | None => React.null
        },
        ("First comment mean time: " ++ data.comment_delay->momentHumanizeDuration)->str,
        ("First review mean time: " ++ data.review_delay->momentHumanizeDuration)->str,
      }
      <GraphWithStats graph stats />
    }
    <QueryRenderCard
      request trigger title tooltip_content icon match childrenBuilder isCentered=false
    />
  }
}

module ChangesMergedDuration = {
  module DurationComplexicityGraph = {
    @react.component @module("./duration_complexity_graph.jsx")
    external make: (~data: array<SearchTypes.change>, ~onClick: string => unit) => React.element =
      "default"
  }
  @react.component
  let make = (~store: Store.t) => {
    let (state, _) = store
    let query = addQuery(state.query, "state:merged")
    let request = {
      ...Store.mkSearchRequest(state, SearchTypes.Query_change),
      query: query,
    }
    let onClick = changeId => {
      let link = "/" ++ state.index ++ "/change/" ++ changeId
      link->RescriptReactRouter.push
    }
    let trigger = state.query
    let title = "Changes merge duration"
    let tooltip_content = "This shows the delay in days to merge by change complexity"
    let icon = <Patternfly.Icons.OutlinedClock />
    let match = resp =>
      switch resp {
      | SearchTypes.Changes(data) => Some(data)
      | _ => None
      }
    let childrenBuilder = (data: Web.SearchTypes.changes) => {
      let graph = <DurationComplexicityGraph data={data.changes->Belt.List.toArray} onClick />
      <GraphWithStats graph stats={list{}} />
    }

    <QueryRenderCard
      request trigger title tooltip_content icon match childrenBuilder isCentered=false
    />
  }
}

module AuthorHistoStats = {
  module CAuthorsHistoChart = {
    @react.component @module("./authors_histo.jsx")
    external make: (
      ~change_histo: array<SearchTypes.histo>,
      ~comment_histo: array<SearchTypes.histo>,
      ~review_histo: array<SearchTypes.histo>,
    ) => React.element = "CAuthorsHistoStats"
  }
  @react.component
  let make = (~store: Store.t) => {
    let (state, _) = store
    let request = Store.mkSearchRequest(state, SearchTypes.Query_active_authors_stats)
    let trigger = state.query
    let title = "Active authors"
    let tooltip_content = "This shows trends of review and comment activities"
    let icon = <Patternfly.Icons.Users />

    let match = resp =>
      switch resp {
      | SearchTypes.Activity_stats(data) => Some(data)
      | _ => None
      }
    let childrenBuilder = (data: Web.SearchTypes.activity_stats) => {
      let graph =
        <CAuthorsHistoChart
          change_histo={data.changes_histo->Belt.List.toArray}
          comment_histo={data.comments_histo->Belt.List.toArray}
          review_histo={data.reviews_histo->Belt.List.toArray}
        />
      let stats = list{
        {("Change authors: " ++ data.change_authors->int32_str)->str},
        {("Review authors: " ++ data.review_authors->int32_str)->str},
        {("Comment authors: " ++ data.comment_authors->int32_str)->str},
      }
      <GraphWithStats graph stats />
    }
    <QueryRenderCard
      request trigger title tooltip_content icon match childrenBuilder isCentered=false
    />
  }
}

@react.component
let make = (~store: Store.t) => {
  <MGrid>
    <MGridItemXl6> <ChangesLifeCycleStats store /> </MGridItemXl6>
    <MGridItemXl6> <ChangesReviewStats store /> </MGridItemXl6>
    <MGridItemXl6> <AuthorHistoStats store /> </MGridItemXl6>
    <MGridItemXl6> <ChangesMergedDuration store /> </MGridItemXl6>
  </MGrid>
}

let default = make
