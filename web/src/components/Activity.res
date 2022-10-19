// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// The activity view
//
open Prelude
open MLink

module GraphWithStats = {
  @react.component
  let make = (~graph: React.element, ~stats: React.element) => {
    <Patternfly.Layout.Stack hasGutter={true}>
      <Patternfly.Layout.StackItem> {stats} </Patternfly.Layout.StackItem>
      <Patternfly.Layout.StackItem>
        <Card> <CardBody> {graph} </CardBody> </Card>
      </Patternfly.Layout.StackItem>
    </Patternfly.Layout.Stack>
  }
}

let item = (title: string, value: React.element) =>
  <ListItem key={title}> <span> <b> {title->str} </b> </span> <span> {value} </span> </ListItem>

module ChangesLifeCycleStats = {
  module ChangesLifeCycleHisto = {
    @react.component @module("./chartjs.jsx")
    external make: (
      ~created: array<MetricTypes.histo_int>,
      ~updated: array<MetricTypes.histo_int>,
      ~merged: array<MetricTypes.histo_int>,
      ~abandoned: array<MetricTypes.histo_int>,
    ) => React.element = "CChangesLifeCycleHisto"
  }
  @react.component
  let make = (
    ~store: Store.t,
    ~extraQuery: option<string>=?,
    ~hideAuthors: option<bool>=?,
    ~isScoped: option<bool>=?,
  ) => {
    let (state, _) = store
    let baseRequest = Store.mkSearchRequest(state, SearchTypes.Query_changes_lifecycle_stats)
    let request = {
      ...baseRequest,
      query: switch extraQuery {
      | Some(ex) => addQuery(baseRequest.query, ex)
      | None => baseRequest.query
      },
    }
    let trigger = state.query ++ extraQuery->Belt.Option.getWithDefault("")
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
      let stats =
        <Layout.Grid md=Column._4>
          <Layout.GridItem>
            <List>
              {Belt.Array.concatMany([
                switch data.created {
                | Some(created) => [item("Changes created: ", created.events_count->int32_str->str)]
                | None => []
                },
                [
                  item(
                    "Changes merged: ",
                    {
                      switch isScoped {
                      | Some(true) =>
                        <MonoLink
                          store
                          filter=""
                          name={data.merged->int32_str}
                          action={SetAuthorScopedTab(MergedChanges)}
                        />
                      | _ =>
                        <MonoLink
                          store filter="state:merged" path="changes" name={data.merged->int32_str}
                        />
                      }
                    },
                  ),
                ],
                [
                  item(
                    "Changes self-merged: ",
                    <MonoLink
                      store
                      filter="state:self_merged"
                      path="changes"
                      name={data.self_merged->int32_str}
                    />,
                  ),
                ],
                [
                  item(
                    "Changes abandoned: ",
                    {
                      switch isScoped {
                      | Some(true) =>
                        <MonoLink
                          store
                          filter=""
                          name={data.abandoned->int32_str}
                          action={SetAuthorScopedTab(AbandonedChanges)}
                        />
                      | _ =>
                        <MonoLink
                          store
                          filter="state:abandoned"
                          path="changes"
                          name={data.abandoned->int32_str}
                        />
                      }
                    },
                  ),
                ],
              ])->React.array}
            </List>
          </Layout.GridItem>
          <List>
            {[
              item("Changes with tests: ", (data.changes_with_tests->float_str ++ "%")->str),
              item("Mean TTM: ", data.ttm_mean->momentHumanizeDuration->str),
              item("TTM Median Deviation: ", data.ttm_variability->momentHumanizeDuration->str),
              item("Changes updates: ", data.updates_of_changes->int32_str->str),
            ]->React.array}
          </List>
          <Layout.GridItem>
            <List>
              {Belt.Array.concatMany([
                [item("Commits by change: ", data.commits_per_change->float_str->str)],
                [item("Iterations by change: ", data.iterations_per_change->float_str->str)],
                switch (hideAuthors->Belt.Option.getWithDefault(false), data.created) {
                | (false, Some(created)) => [
                    item("Changes authors: ", created.authors_count->int32_str->str),
                  ]
                | _ => []
                },
              ])->React.array}
            </List>
          </Layout.GridItem>
        </Layout.Grid>
      <GraphWithStats graph stats />
    }
    <QueryRenderCard request trigger title tooltip_content icon match childrenBuilder />
  }
}

module ChangesReviewStats = {
  module CChangesReviewHisto = {
    @react.component @module("./chartjs.jsx")
    external make: (
      ~comment_histo: array<MetricTypes.histo_int>,
      ~review_histo: array<MetricTypes.histo_int>,
    ) => React.element = "CChangeReviewEventsHisto"
  }
  @react.component
  let make = (~store: Store.t, ~extraQuery: option<string>=?, ~hideAuthors: option<bool>=?) => {
    let (state, _) = store
    let baseRequest = Store.mkSearchRequest(state, SearchTypes.Query_changes_review_stats)
    let request = {
      ...baseRequest,
      query: switch extraQuery {
      | Some(ex) => addQuery(baseRequest.query, ex)
      | None => baseRequest.query
      },
    }
    let trigger = state.query ++ extraQuery->Belt.Option.getWithDefault("")
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
      let stats =
        <Layout.Grid md=Column._4>
          <Layout.GridItem>
            <List>
              {Belt.Array.concatMany([
                switch data.review_count {
                | Some(review) => [item("Changes reviewed: ", review.events_count->int32_str->str)]
                | None => []
                },
                [item("1st review mean time: ", data.review_delay->momentHumanizeDuration->str)],
                switch (hideAuthors->Belt.Option.getWithDefault(false), data.review_count) {
                | (false, Some(review)) => [
                    item("Reviews authors: ", review.authors_count->int32_str->str),
                  ]
                | _ => []
                },
              ])->React.array}
            </List>
          </Layout.GridItem>
          <Layout.GridItem>
            <List>
              {Belt.Array.concatMany([
                switch data.comment_count {
                | Some(comment) => [
                    item("Changes commented: ", comment.events_count->int32_str->str),
                  ]
                | None => []
                },
                [item("1st comment mean time: ", data.comment_delay->momentHumanizeDuration->str)],
                switch (hideAuthors->Belt.Option.getWithDefault(false), data.comment_count) {
                | (false, Some(comment)) => [
                    item("Comments authors: ", comment.authors_count->int32_str->str),
                  ]
                | _ => []
                },
              ])->React.array}
            </List>
          </Layout.GridItem>
        </Layout.Grid>
      <GraphWithStats graph stats />
    }
    <QueryRenderCard request trigger title tooltip_content icon match childrenBuilder />
  }
}

module ChangesMergedDuration = {
  module DurationComplexityGraph = {
    @react.component @module("./chartjs.jsx")
    external make: (~data: array<SearchTypes.change>, ~onClick: string => unit) => React.element =
      "ChangesReviewStats"
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
      let graph = <DurationComplexityGraph data={data.changes->Belt.List.toArray} onClick />
      <GraphWithStats graph stats=React.null />
    }

    <QueryRenderCard request trigger title tooltip_content icon match childrenBuilder />
  }
}

module AuthorHistoStats = {
  module CAuthorsHistoChart = {
    @react.component @module("./chartjs.jsx")
    external make: (
      ~change_histo: array<MetricTypes.histo_int>,
      ~comment_histo: array<MetricTypes.histo_int>,
      ~review_histo: array<MetricTypes.histo_int>,
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

      let stats =
        <Layout.Grid md=Column._4>
          <Layout.GridItem>
            <List>
              {[
                item("Change authors: ", data.change_authors->int32_str->str),
                item("Review authors: ", data.review_authors->int32_str->str),
                item("Comment authors: ", data.comment_authors->int32_str->str),
              ]->React.array}
            </List>
          </Layout.GridItem>
        </Layout.Grid>
      <GraphWithStats graph stats />
    }
    <QueryRenderCard request trigger title tooltip_content icon match childrenBuilder />
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
