open Prelude
open Change

@react.component
let make = (~store: Store.t, ~change: string) => {
  let (state, _) = store
  let request = {
    ...Store.mkSearchRequest(state, SearchTypes.Query_change_and_events),
    change_id: change,
  }

  <div className="container">
    <QueryRender
      request
      trigger={state.query}
      render={resp =>
        switch resp {
        | SearchTypes.Change_events({change, events}) =>
          <ChangeDetailView
            store change={change->Belt.Option.getExn} events={events->Belt.List.toArray}
          />
        | _ => /* Response does not match request */ React.null
        }}
    />
  </div>
}

let default = make
