// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// The activity view
//
open Prelude
open MLink

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
    let childrenBuilder = (data: Web.SearchTypes.lifecycle_stats) =>
      <Patternfly.Layout.Stack hasGutter={true}>
        <Patternfly.Layout.StackItem>
          <Card>
            <CardBody>
              <ChangesLifeCycleHisto
                created={data.created_histo->Belt.List.toArray}
                updated={data.updated_histo->Belt.List.toArray}
                merged={data.merged_histo->Belt.List.toArray}
                abandoned={data.abandoned_histo->Belt.List.toArray}
              />
            </CardBody>
          </Card>
        </Patternfly.Layout.StackItem>
        <Patternfly.Layout.StackItem>
          <Patternfly.Layout.Grid hasGutter={false}>
            {switch data.created {
            | Some(created) =>
              <Patternfly.Layout.GridItem md=Column._6 xl=Column._4>
                <Card isCompact={true}>
                  <CardBody>
                    {created.events_count->int32_str ++
                    " changes created by " ++
                    created.authors_count->int32_str ++ " authors"}
                  </CardBody>
                </Card>
              </Patternfly.Layout.GridItem>
            | None => React.null
            }}
            <Patternfly.Layout.GridItem md=Column._6 xl=Column._4>
              <Card isCompact={true}>
                <CardBody>
                  <MonoLink
                    store
                    filter="state:abandoned"
                    path="changes"
                    name={data.abandoned->int32_str ++ " changes abandoned"}
                  />
                </CardBody>
              </Card>
            </Patternfly.Layout.GridItem>
            <Patternfly.Layout.GridItem md=Column._6 xl=Column._4>
              <Card isCompact={true}>
                <CardBody>
                  <MonoLink
                    store
                    filter="state:merged"
                    path="changes"
                    name={data.merged->int32_str ++ " changes merged"}
                  />
                </CardBody>
              </Card>
            </Patternfly.Layout.GridItem>
            <Patternfly.Layout.GridItem md=Column._6 xl=Column._4>
              <Card isCompact={true}>
                <CardBody>
                  <MonoLink
                    store
                    filter="state:self_merged"
                    path="changes"
                    name={data.self_merged->int32_str ++ " changes self merged"}
                  />
                </CardBody>
              </Card>
            </Patternfly.Layout.GridItem>
            <Patternfly.Layout.GridItem md=Column._6 xl=Column._4>
              <Card isCompact={true}>
                <CardBody>
                  {"Mean Time To Merge: " ++ data.ttm_mean->momentHumanizeDuration}
                </CardBody>
              </Card>
            </Patternfly.Layout.GridItem>
            <Patternfly.Layout.GridItem md=Column._6 xl=Column._4>
              <Card isCompact={true}>
                <CardBody>
                  {"TTM Median Deviation: " ++ data.ttm_variability->momentHumanizeDuration}
                </CardBody>
              </Card>
            </Patternfly.Layout.GridItem>
            <Patternfly.Layout.GridItem md=Column._6 xl=Column._4>
              <Card isCompact={true}>
                <CardBody> {data.updates_of_changes->int32_str ++ " updates of changes"} </CardBody>
              </Card>
            </Patternfly.Layout.GridItem>
            <Patternfly.Layout.GridItem md=Column._6 xl=Column._4>
              <Card isCompact={true}>
                <CardBody>
                  {"Changes with tests: " ++ data.changes_with_tests->Belt.Float.toString ++ "%"}
                </CardBody>
              </Card>
            </Patternfly.Layout.GridItem>
            <Patternfly.Layout.GridItem md=Column._6 xl=Column._4>
              <Card isCompact={true}>
                <CardBody>
                  {data.iterations_per_change->Belt.Float.toString ++ " iterations per change"}
                </CardBody>
              </Card>
            </Patternfly.Layout.GridItem>
            <Patternfly.Layout.GridItem md=Column._6 xl=Column._4>
              <Card isCompact={true}>
                <CardBody>
                  {data.commits_per_change->Belt.Float.toString ++ " commits per change"}
                </CardBody>
              </Card>
            </Patternfly.Layout.GridItem>
          </Patternfly.Layout.Grid>
        </Patternfly.Layout.StackItem>
      </Patternfly.Layout.Stack>
    <QueryRenderCard
      request trigger title tooltip_content icon match childrenBuilder isCentered=false
    />
  }
}

module CChangesReviewStats = {
  @react.component @module("./changes_review.jsx")
  external make: (
    ~data: SearchTypes.review_stats,
    ~comment_histo: array<SearchTypes.histo>,
    ~review_histo: array<SearchTypes.histo>,
  ) => React.element = "ChangesReviewStats"
}

module ChangesReviewStats = {
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
    let childrenBuilder = (data: Web.SearchTypes.review_stats) =>
      <CChangesReviewStats
        data
        comment_histo={data.comment_histo->Belt.List.toArray}
        review_histo={data.review_histo->Belt.List.toArray}
      />
    <QueryRenderCard
      request trigger title tooltip_content icon match childrenBuilder isCentered=false
    />
  }
}

module DurationComplexicityGraph = {
  @react.component @module("./duration_complexity_graph.jsx")
  external make: (~data: array<SearchTypes.change>, ~onClick: string => unit) => React.element =
    "default"
}

module ChangesMergedDuration = {
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
    let childrenBuilder = (data: Web.SearchTypes.changes) =>
      <DurationComplexicityGraph data={data.changes->Belt.List.toArray} onClick />

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
    let childrenBuilder = (data: Web.SearchTypes.activity_stats) =>
      <Patternfly.Layout.Stack hasGutter={true}>
        <Patternfly.Layout.StackItem>
          <Card>
            <CardBody>
              <CAuthorsHistoChart
                change_histo={data.changes_histo->Belt.List.toArray}
                comment_histo={data.comments_histo->Belt.List.toArray}
                review_histo={data.reviews_histo->Belt.List.toArray}
              />
            </CardBody>
          </Card>
        </Patternfly.Layout.StackItem>
        <Patternfly.Layout.StackItem>
          <Patternfly.Layout.Grid hasGutter={false}>
            <Patternfly.Layout.GridItem md=Column._6 xl=Column._4>
              <Card isCompact={true}>
                <CardBody> {"Change authors: " ++ data.change_authors->int32_str} </CardBody>
              </Card>
            </Patternfly.Layout.GridItem>
            <Patternfly.Layout.GridItem md=Column._6 xl=Column._4>
              <Card isCompact={true}>
                <CardBody> {"Review authors: " ++ data.review_authors->int32_str} </CardBody>
              </Card>
            </Patternfly.Layout.GridItem>
            <Patternfly.Layout.GridItem md=Column._6 xl=Column._4>
              <Card isCompact={true}>
                <CardBody> {"Comment authors: " ++ data.comment_authors->int32_str} </CardBody>
              </Card>
            </Patternfly.Layout.GridItem>
          </Patternfly.Layout.Grid>
        </Patternfly.Layout.StackItem>
      </Patternfly.Layout.Stack>
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
