// A module to manage the pinned changes

type t = MonoIndexedDB.PinnedChangeSchema.t
module Table = Dexie.Table.MakeTable(MonoIndexedDB.PinnedChangeSchema)

let remove = (dexie: Dexie.Database.t, key: string) => dexie->Table.delete(key)->ignore

type changeStatus = Pinned | Unpinned
type status<'a> = (changeStatus, 'a)
type changeArray = array<status<SearchTypes.change>>
type change = status<SearchTypes.change>
type dispatch = (SearchTypes.change => unit, SearchTypes.change => unit)

// Take a dexie db and a change,
// and return a promise with the pinned status.
let getStatus: (Dexie.Database.t, SearchTypes.change) => Promise.t<status<SearchTypes.change>> = (
  dexie,
  change,
) =>
  dexie
  ->Table.getById(change.change_id)
  ->Promise.then(pinnedM =>
    (
      switch pinnedM {
      | Some(_) => Pinned
      | _ => Unpinned
      },
      change,
    )->Promise.resolve
  )

let simpleGetStatus: (changeArray, SearchTypes.change) => changeStatus = (xs, change) => {
  let pinnedM =
    Belt.Array.keep(xs, ((_, change')) => change.change_id == change'.change_id)->Belt.Array.get(0)
  switch pinnedM {
  | Some((pinnedStatus, _)) => pinnedStatus
  | _ => Unpinned
  }
}

let style = pinnedStatus =>
  switch pinnedStatus {
  | Pinned => ReactDOM.Style.make(~background="papayawhip", ())
  | Unpinned => ReactDOM.Style.make()
  }

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
  let (pinned, setPinned) = React.useState(_ => [])

  // Lookup change in indexed db
  React.useEffect1(() => {
    xs->getStatusFromChanges(dexie)->Promise.then(xs => setPinned(_ => xs)->Promise.resolve)->ignore
    None
  }, [xs])

  let pinChange = (change: SearchTypes.change) => {
    dexie
    ->Table.put({id: change.change_id, ctype: #Change})
    ->Promise.then(_ => setPinned(setStatus(Pinned, change))->Promise.resolve)
    ->ignore
  }

  let unPinChange = (change: SearchTypes.change) => {
    dexie
    ->Table.delete(change.change_id)
    ->Promise.then(_ => setPinned(setStatus(Unpinned, change))->Promise.resolve)
    ->ignore
  }

  (pinned, (pinChange, unPinChange))
}
