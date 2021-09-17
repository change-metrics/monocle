// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// The new contributors view component
//

open Prelude

@react.component
let make = (~store: Store.t) => {
  let (state, _) = store
  let (limit, setLimit) = React.useState(() => 25)
  let limit_values = list{10, 25, 50, 100, 500}
  let columnNames = ["Author", "Change count"]
  let tooltip_content = "This shows the list of authors that are new contributors of changes"
  let request = {
    ...Store.mkSearchRequest(state, SearchTypes.Query_new_changes_authors),
    limit: limit->Int32.of_int,
  }
  let trigger = state.query ++ limit->string_of_int
  let limitSelector = <LimitSelector limit setLimit default=25 values=limit_values />
  let title = "New change' authors"
  let icon = <Patternfly.Icons.Plus />
  let match = resp =>
    switch resp {
    | SearchTypes.New_authors(na) => Some(na)
    | _ => None
    }
  let childrenBuilder = (data: Web.SearchTypes.terms_count) =>
    <TopTermsTable items={data.termcount} columnNames />
  <QueryRenderCard request trigger title tooltip_content icon limitSelector match childrenBuilder />
}

let default = make
