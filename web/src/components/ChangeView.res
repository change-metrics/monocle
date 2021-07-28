open Prelude

module CChange = {
  @react.component @module("./change.jsx")
  external make: (
    ~index: string,
    ~change: SearchTypes.change,
    ~events: array<SearchTypes.change_event>,
  ) => React.element = "default"
}

@react.component
let make = (~store: Store.t, ~change: string) => {
  let (state, _) = store
  let index = state.index
  let request = {
    ...Store.mkSearchRequest(state, SearchTypes.Query_change_and_events),
    change_id: change,
  }

  <div className="container">
    {switch useAutoGetOn(() => WebApi.Search.query(request), state.query) {
    | None => <Spinner />
    | Some(Error(title)) => <Alert variant=#Danger title />
    | Some(Ok(SearchTypes.Error(err))) =>
      <Alert
        title={err.message ++ " at " ++ string_of_int(Int32.to_int(err.position))} variant=#Danger
      />
    | Some(Ok(SearchTypes.Change_events({change, events}))) =>
      <CChange index change={change->Belt.Option.getExn} events={events->Belt.List.toArray} />
    | Some(Ok(_)) => /* Response does not match request */ React.null
    }}
  </div>
}

let default = make
