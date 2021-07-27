// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// The new contributors view component
//

open Prelude

@react.component
let make = (~store: Store.t) => {
  let (state, _) = store
  let index = state.index
  let query = state.query
  let (limit, setLimit) = React.useState(() => 25)
  let limit_values = list{10, 25, 50, 100, 500}
  let columnNames = ["Author", "Change count"]
  let tooltip_content = "This shows the list of authors that are new contributors of changes"
  let request = {
    SearchTypes.index: index,
    query: query,
    username: "",
    query_type: SearchTypes.Query_new_changes_authors,
    // Not handled server side
    order: None,
    limit: limit->Int32.of_int,
  }
  <div>
    {switch useAutoGetOn(() => WebApi.Search.query(request), query ++ limit->string_of_int) {
    | None => <Spinner />
    | Some(Error(title)) => <Alert variant=#Danger title />
    | Some(Ok(SearchTypes.Error(err))) =>
      <Alert
        title={err.message ++ " at " ++ string_of_int(Int32.to_int(err.position))} variant=#Danger
      />
    | Some(Ok(SearchTypes.New_authors(na))) =>
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
          <CardBody> <MGrid> <TopTermsTable items={na.termcount} columnNames /> </MGrid> </CardBody>
        </Card>
      </MCenteredContent>
    | Some(Ok(_)) => React.null
    }}
  </div>
}

let default = make
