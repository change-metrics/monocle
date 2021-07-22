// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// The activity view
//
open Prelude

module CChangesLifeCycleStats = {
  @react.component @module("./changes_lifecycle.jsx")
  external make: (~index: string) => React.element = "CChangesLifeCycleStats"
}

module ChangesLifeCycleStats = {
  @react.component
  let make = (~store: Store.t) => {
    let (state, _) = store
    <CChangesLifeCycleStats index={state.index} />
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
    let request = {
      SearchTypes.index: state.index,
      query: state.query,
      username: "",
      query_type: SearchTypes.Query_changes_review_stats,
      order: None,
      limit: 0->Int32.of_int,
    }

    {
      switch useAutoGetOn(() => WebApi.Search.query(request), state.query) {
      | None => <Spinner />
      | Some(Error(title)) => <Alert variant=#Danger title />
      | Some(Ok(SearchTypes.Error(err))) =>
        <Alert
          title={err.message ++ " at " ++ string_of_int(Int32.to_int(err.position))} variant=#Danger
        />
      | Some(Ok(SearchTypes.Review_stats(data))) =>
        <CChangesReviewStats
          data
          comment_histo={data.comment_histo->Belt.List.toArray}
          review_histo={data.review_histo->Belt.List.toArray}
        />
      | Some(Ok(_)) => /* Response does not match request */ React.null
      }
    }
  }
}

@react.component
let make = (~store: Store.t) => {
  <div className="container">
    <MStack>
      <MStackItem> <ChangesLifeCycleStats store /> </MStackItem>
      <MStackItem> <ChangesReviewStats store /> </MStackItem>
    </MStack>
  </div>
}

let default = make
