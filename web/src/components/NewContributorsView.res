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
  <div>
    <Search.QueryRender
      request
      trigger={state.query ++ limit->string_of_int}
      render={resp =>
        switch resp {
        | SearchTypes.New_authors(na) =>
          <MCenteredContent>
            <Card isCompact=true>
              <CardTitle>
                <MGrid>
                  <MGridItemXl9>
                    <Title headingLevel=#H3>
                      <Tooltip content=tooltip_content> <Patternfly.Icons.Plus /> </Tooltip>
                      {(" " ++ "New change' authors")->str}
                    </Title>
                  </MGridItemXl9>
                  <MGridItemXl3>
                    <LimitSelector limit setLimit default=25 values=limit_values />
                  </MGridItemXl3>
                </MGrid>
              </CardTitle>
              <CardBody>
                <MGrid> <TopTermsTable items={na.termcount} columnNames /> </MGrid>
              </CardBody>
            </Card>
          </MCenteredContent>
        | _ => React.null
        }}
    />
  </div>
}

let default = make
