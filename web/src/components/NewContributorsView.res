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
  let limitSelector = <LimitSelector limit setLimit default=25 values=limit_values />
  let title = "New change' authors"
  let icon = <Patternfly.Icons.Plus />
  <Search.QueryRender
    request
    trigger={state.query ++ limit->string_of_int}
    render={resp =>
      switch resp {
      | SearchTypes.New_authors(na) =>
        <MCenteredContent>
          <MonoCard title tooltip_content icon limitSelector>
            <TopTermsTable items={na.termcount} columnNames />
          </MonoCard>
        </MCenteredContent>
      | _ => React.null
      }}
  />
}

let default = make
