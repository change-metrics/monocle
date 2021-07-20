// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// The most active authors view component
//

open Prelude

module TopTermsTable = {
  @react.component
  let make = (~terms: list<SearchTypes.term_count>) => {
    let columnNames = ["Name", "Count"]

    let isOrdered = (first: SearchTypes.term_count, second: SearchTypes.term_count, index) =>
      switch index {
      | 0 => first.term < second.term
      | 1 => first.count < second.count
      | _ => false
      }
    let formatters: list<SearchTypes.term_count => React.element> = list{
      item => item.term->str,
      item => item.count->int32_str->str,
    }

    <SortableTable
      items=terms caption="Top terms" defaultSortedColumn=1 columnNames isOrdered formatters
    />
  }
}

module LimitSelector = {
  @react.component
  let make = (~limit: int, ~setLimit: (int => int) => unit) => {
    let setLimit' = str => {
      let v = str == "" ? 0 : str->int_of_string
      setLimit(_ => v)
    }
    <Patternfly.Layout.Bullseye>
      <MSelect
        placeholder={"Set limit"}
        options={list{10, 25, 50, 100, 500}->Belt.List.map(string_of_int)}
        multi={false}
        value={limit > 0 ? limit->string_of_int : ""}
        valueChanged={setLimit'}
      />
    </Patternfly.Layout.Bullseye>
  }
}

@react.component
let make = (~store: Store.t) => {
  let (state, _) = store
  let index = state.index
  let query = state.query
  let (limit, setLimit) = React.useState(() => 10)
  let request = {
    SearchTypes.index: index,
    query: query,
    username: "",
    query_type: SearchTypes.Query_top_authors_changes_created,
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
    | Some(Ok(SearchTypes.Top_authors(tsc))) =>
      <MCenteredContent>
        <Card isCompact=true>
          <CardTitle>
            <MGrid>
              <MGridItem> {"Most active authors - Changes Created"->str} </MGridItem>
              <MGridItem> <LimitSelector limit setLimit /> </MGridItem>
            </MGrid>
          </CardTitle>
          <CardBody> <TopTermsTable terms=tsc.termcount /> </CardBody>
        </Card>
      </MCenteredContent>
    | Some(Ok(_)) => React.null
    }}
  </div>
}
let default = make
