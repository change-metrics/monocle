include Prelude

module View = {
  @react.component
  let make = (~store: Store.t) => {
    let (hiddens, setHiddens) = React.useState(() => [])
    let (state, _) = store
    let reload = () =>
      LocalStore.getAll(state.dexie)
      ->Promise.then(xs => setHiddens(_ => xs)->Promise.resolve)
      ->ignore

    React.useEffect0(() => {
      reload()
      None
    })
    <>
      <h3> {"Hidden changes"->str} </h3>
      <ul>
        {hiddens
        ->Belt.Array.mapWithIndex((idx, hidden) =>
          <li key={string_of_int(idx)}>
            {hidden.id->str}
            <a
              style={ReactDOM.Style.make(
                ~cursor="pointer",
                ~color="#007bff",
                ~paddingLeft="5px",
                (),
              )}
              onClick={_ => LocalStore.remove(state.dexie, hidden.id)->reload}>
              {"Remove"->str}
            </a>
          </li>
        )
        ->React.array}
      </ul>
    </>
  }
}
