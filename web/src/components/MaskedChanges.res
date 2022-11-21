// A module to manage the masked changes

type t = MonoIndexedDB.MaskedChangeSchema.t
module Table = Dexie.Table.MakeTable(MonoIndexedDB.MaskedChangeSchema)

let getAll: Dexie.Database.t => Promise.t<array<t>> = dexie => {
  dexie->Table.findByCriteria({"ctype": #Change})->Dexie.Collection.toArray
}

let remove = (dexie: Dexie.Database.t, key: string) => dexie->Table.delete(key)->ignore

type changeStatus = Masked | Unmasked
type status<'a> = (changeStatus, 'a)
type changeArray = array<status<SearchTypes.change>>
type change = status<SearchTypes.change>
type dispatch = (SearchTypes.change => unit, SearchTypes.change => unit)

// Take a dexie db and a change,
// and return a promise with the masked status.
let getStatus: (Dexie.Database.t, SearchTypes.change) => Promise.t<status<SearchTypes.change>> = (
  dexie,
  change,
) =>
  dexie
  ->Table.getById(change.change_id)
  ->Promise.then(maskedM =>
    (
      switch maskedM {
      | Some(_) => Masked
      | _ => Unmasked
      },
      change,
    )->Promise.resolve
  )

let simpleGetStatus: (changeArray, SearchTypes.change) => changeStatus = (xs, change) => {
  let maskedM =
    Belt.Array.keep(xs, ((_, change')) => change.change_id == change'.change_id)->Belt.Array.get(0)
  switch maskedM {
  | Some((maskedStatus, _)) => maskedStatus
  | _ => Unmasked
  }
}

// Take a list of change, a dexie db,
// Then annotate each change,
let getStatusFromChanges: (
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
  let (masked, setMasked) = React.useState(_ => [])

  // Lookup change in indexed db
  React.useEffect1(() => {
    xs->getStatusFromChanges(dexie)->Promise.then(xs => setMasked(_ => xs)->Promise.resolve)->ignore
    None
  }, [xs])

  let maskChange = (change: SearchTypes.change) => {
    dexie
    ->Table.put({id: change.change_id, ctype: #Change})
    ->Promise.then(_ => setMasked(setStatus(Masked, change))->Promise.resolve)
    ->ignore
  }

  let unMaskChange = (change: SearchTypes.change) => {
    dexie
    ->Table.delete(change.change_id)
    ->Promise.then(_ => setMasked(setStatus(Unmasked, change))->Promise.resolve)
    ->ignore
  }

  (masked, (maskChange, unMaskChange))
}
