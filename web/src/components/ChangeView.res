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
    <Search.QueryRender
      request
      trigger={state.query}
      render={resp =>
        switch resp {
        | SearchTypes.Change_events({change, events}) =>
          <CChange index change={change->Belt.Option.getExn} events={events->Belt.List.toArray} />
        | _ => /* Response does not match request */ React.null
        }}
    />
  </div>
}

let default = make
