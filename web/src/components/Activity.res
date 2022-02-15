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
      </Patternfly.Layout.StackItem>
      <Patternfly.Layout.StackItem>
        <Card> <CardBody> {graph} </CardBody> </Card>
      </Patternfly.Layout.StackItem>
    </Patternfly.Layout.Stack>
  }
}

let box = (title: string, value: React.element) =>
  <div> <span> <b> {title->str} </b> </span> <span> {value} </span> </div>

let getTrigger = (store: Store.t) => {
  let (state, _) = store
  state.query ++
  switch state.author_scoped {
  | Some(Group(name)) => "group-" ++ name
  | Some(Author(name)) => "author-" ++ name
  | None => ""
  }
}

module ChangesLifeCycleStats = {
  module ChangesLifeCycleHisto = {
    @react.component @module("./chartjs.jsx")
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
    let trigger = getTrigger(store)
    let title = "Changes lifecycle stats"
    let tooltip_content = "This shows trends of change related metrics such as the evolution of the amount of change created"
    let icon = <Patternfly.Icons.Running />
    let displayAuthors = switch state.author_scoped {
    | Some(Author(_)) => false
    | _ => true
    }

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
        switch data.created {
        | Some(created) => box("Changes created: ", created.events_count->int32_str->str)
        | None => React.null
        },
        box(
          "Changes merged: ",
          <MonoLink store filter="state:merged" path="changes" name={data.merged->int32_str} />,
        ),
        box(
          "Changes self-merged: ",
          <MonoLink
            store filter="state:self_merged" path="changes" name={data.self_merged->int32_str}
          />,
        ),
        box(
          "Changes abandoned: ",
          <MonoLink
            store filter="state:abandoned" path="changes" name={data.abandoned->int32_str}
          />,
        ),
        box("Mean TTM: ", data.ttm_mean->momentHumanizeDuration->str),
        box("TTM Median Deviation: ", data.ttm_variability->momentHumanizeDuration->str),
        box("Changes updates: ", data.updates_of_changes->int32_str->str),
        box("Commits by change: ", data.commits_per_change->float_str->str),
        box("Iterations by change: ", data.iterations_per_change->float_str->str),
        box("Changes with tests: ", (data.changes_with_tests->float_str ++ "%")->str),
      }
      let authorsStats = displayAuthors
        ? switch data.created {
          | Some(created) => list{box("Changes authors: ", created.authors_count->int32_str->str)}
          | None => list{}
          }
        : list{}
      let stats = Belt.List.concat(stats, authorsStats)
      <GraphWithStats graph stats />
    }
    <QueryRenderCard
      request trigger title tooltip_content icon match childrenBuilder isCentered=false
    />
  }
}

module ChangesReviewStats = {
  module CChangesReviewHisto = {
    @react.component @module("./chartjs.jsx")
    external make: (
      ~comment_histo: array<SearchTypes.histo>,
      ~review_histo: array<SearchTypes.histo>,
    ) => React.element = "CChangeReviewEventsHisto"
  }
  @react.component
  let make = (~store: Store.t) => {
    let (state, _) = store
    let request = Store.mkSearchRequest(state, SearchTypes.Query_changes_review_stats)
    let trigger = getTrigger(store)
    let title = "Changes review stats"
    let tooltip_content = "This shows trends of reviews and comments"
    let icon = <Patternfly.Icons.OutlinedComments />
    let displayAuthors = switch state.author_scoped {
    | Some(Author(_)) => false
    | _ => true
    }

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
        switch data.review_count {
        | Some(review) => box("Changes reviewed: ", review.events_count->int32_str->str)
        | None => React.null
        },
        box("1st review mean time: ", data.review_delay->momentHumanizeDuration->str),
        switch data.comment_count {
        | Some(comment) => box("Changes commented: ", comment.events_count->int32_str->str)
        | None => React.null
        },
        box("1st comment mean time: ", data.comment_delay->momentHumanizeDuration->str),
      }
      let authorsStats = displayAuthors
        ? Belt.List.concat(
            switch data.review_count {
            | Some(review) => list{box("Reviews authors: ", review.authors_count->int32_str->str)}
            | None => list{}
            },
            switch data.comment_count {
            | Some(comment) => list{
                box("Comments authors: ", comment.authors_count->int32_str->str),
              }
            | None => list{}
            },
          )
        : list{}

      let stats = Belt.List.concat(stats, authorsStats)
      <GraphWithStats graph stats />
    }
    <QueryRenderCard
      request trigger title tooltip_content icon match childrenBuilder isCentered=false
    />
  }
}

module ChangesMergedDuration = {
  module DurationComplexicityGraph = {
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
    @react.component @module("./chartjs.jsx")
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
        {box("Change authors: ", data.change_authors->int32_str->str)},
        {box("Review authors: ", data.review_authors->int32_str->str)},
        {box("Comment authors: ", data.comment_authors->int32_str->str)},
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
