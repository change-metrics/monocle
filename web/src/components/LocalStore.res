module HiddenChangeSchema = {
  type id = string
  type t = {id: string, hidden_at: Js.Date.t}
  let tableName = "hidden"
}

module HiddenChange = Dexie.Table.MakeTable(HiddenChangeSchema)

let mkDexie: unit => Dexie.Database.t = () => {
  let dexie = Dexie.Database.make("Monocle")
  let schema = [("hidden", "&id")]
  dexie->Dexie.Database.version(1)->Dexie.Version.stores(schema)->ignore
  dexie
}

// Take a dexie db and a change,
// and return a promise with an option set to None when the change is hidden
let keepVisible: (Dexie.Database.t, SearchTypes.change) => Promise.t<option<SearchTypes.change>> = (
  dexie,
  x,
) =>
  dexie
  ->HiddenChange.getById(x.change_id)
  ->Promise.then(hiddenM =>
    switch hiddenM {
    | Some(hidden) if x.updated_at->Prelude.getDate <= hidden.hidden_at => None
    | _ => Some(x)
    }->Promise.resolve
  )

// Take a list of change, a dexie db,
// Then lookup each change,
// and return a promise with the one that are not hidden.
let removeHiddenFromArray: (
  array<SearchTypes.change>,
  Dexie.Database.t,
) => Promise.t<array<SearchTypes.change>> = (xs, dexie) => {
  xs
  ->Belt.Array.map(keepVisible(dexie))
  ->Promise.all
  ->Promise.then(xs => xs->Belt.Array.keepMap(x => x)->Promise.resolve)
}

// A hook that returns a list of visible changes
let useHidden = (dexie, xs) => {
  let (hidden, setHidden) = React.useState(_ => [])

  // Lookup change in indexed db
  React.useEffect1(() => {
    xs
    ->removeHiddenFromArray(dexie)
    ->Promise.then(xs => setHidden(_ => xs)->Promise.resolve)
    ->ignore
    None
  }, [xs])

  let hideChange = (change: SearchTypes.change) => {
    dexie
    ->HiddenChange.put({id: change.change_id, hidden_at: Prelude.getCurrentTime()})
    ->Promise.then(_ => {
      setHidden(xs => xs->Belt.Array.keep(x => x.change_id != change.change_id))->Promise.resolve
    })
    ->ignore
  }

  (hidden, hideChange)
}
