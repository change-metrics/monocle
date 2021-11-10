// A module to manage the hidden changes

type t = MonoIndexedDB.HiddenChangeSchema.t
module Table = Dexie.Table.MakeTable(MonoIndexedDB.HiddenChangeSchema)

let getAll: Dexie.Database.t => Promise.t<array<t>> = dexie => {
  dexie->Table.findByCriteria({"ctype": #Change})->Dexie.Collection.toArray
}

let remove = (dexie: Dexie.Database.t, key: string) => dexie->Table.delete(key)->ignore

// Take a dexie db and a change,
// and return a promise with an option set to None when the change is hidden
let keepVisible: (Dexie.Database.t, SearchTypes.change) => Promise.t<option<SearchTypes.change>> = (
  dexie,
  x,
) =>
  dexie
  ->Table.getById(x.change_id)
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
let use = (dexie, xs) => {
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
    ->Table.put({id: change.change_id, hidden_at: Prelude.getCurrentTime(), ctype: #Change})
    ->Promise.then(_ => {
      setHidden(xs => xs->Belt.Array.keep(x => x.change_id != change.change_id))->Promise.resolve
    })
    ->ignore
  }

  (hidden, hideChange)
}
