// A module to manage the hidden changes

type t = MonoIndexedDB.HiddenChangeSchema.t
module Table = Dexie.Table.MakeTable(MonoIndexedDB.HiddenChangeSchema)

let getAll: Dexie.Database.t => Promise.t<array<t>> = dexie => {
  dexie->Table.findByCriteria({"ctype": #Change})->Dexie.Collection.toArray
}

let remove = (dexie: Dexie.Database.t, key: string) => dexie->Table.delete(key)->ignore

type changeStatus = Hidden | Visible | Updated
type status<'a> = (changeStatus, 'a)
type changeArray = array<status<SearchTypes.change>>
type change = status<SearchTypes.change>
type dispatch = (SearchTypes.change => unit, SearchTypes.change => unit)

// Take a dexie db and a change,
// and return a promise with the hidden status.
let getStatus: (Dexie.Database.t, SearchTypes.change) => Promise.t<status<SearchTypes.change>> = (
  dexie,
  change,
) =>
  dexie
  ->Table.getById(change.change_id)
  ->Promise.then(hiddenM =>
    (
      switch hiddenM {
      | Some(hidden) if change.updated_at->Prelude.getDate <= hidden.hidden_at => Hidden
      | Some(_) => Updated
      | _ => Visible
      },
      change,
    )->Promise.resolve
  )

// Take a list of change, a dexie db,
// Then annotate each change,
let removeHiddenFromArray: (
  array<SearchTypes.change>,
  Dexie.Database.t,
) => Promise.t<changeArray> = (xs, dexie) => {
  xs->Belt.Array.map(getStatus(dexie))->Promise.all
}

let setStatus: (changeStatus, SearchTypes.change, changeArray) => changeArray = (
  newStatus,
  change,
  xs,
) =>
  xs->Belt.Array.map(((prevStatus, x)) =>
    switch x.change_id == change.change_id {
    | true => (newStatus, x)
    | false => (prevStatus, x)
    }
  )

// A hook that returns a list of annotate changes changes
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
    ->Promise.then(_ => setHidden(setStatus(Hidden, change))->Promise.resolve)
    ->ignore
  }

  let revealChange = (change: SearchTypes.change) => {
    dexie
    ->Table.delete(change.change_id)
    ->Promise.then(_ => setHidden(setStatus(Visible, change))->Promise.resolve)
    ->ignore
  }

  (hidden, (hideChange, revealChange))
}
