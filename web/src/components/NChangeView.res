open Prelude

@react.component
let make = (~store: Store.t) => {
  let (state, _) = store
  let query = addQuery(state.query, state.filter)
  let request = {
    ...Store.mkSearchRequest(state, SearchTypes.Query_change),
    query: query,
  }

  <div>
    <Patternfly.Layout.Bullseye> <Search.Filter store /> </Patternfly.Layout.Bullseye>
    {switch useAutoGetOn(() => WebApi.Search.query(request), query ++ state.order->orderToQS) {
    | None => <Spinner />
    | Some(Error(title)) => <Alert variant=#Danger title />
    | Some(Ok(SearchTypes.Error(err))) =>
      <Alert
        title={err.message ++ " at " ++ string_of_int(Int32.to_int(err.position))} variant=#Danger
      />
    | Some(Ok(SearchTypes.Changes(items))) => {
        let changes = items.changes->Belt.List.toArray
        switch changes->Belt.Array.length {
        | 0 => <p> {"No changes matched"->str} </p>
        | _ =>
          <Patternfly.DataList isCompact={true}>
            {changes
            ->Belt.Array.map(change => <Change.DataItem store key={change.url} change={change} />)
            ->React.array}
          </Patternfly.DataList>
        }
      }
    | Some(Ok(_)) => <Alert title={"Invalid response"} />
    }}
  </div>
}

let default = make
