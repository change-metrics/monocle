// A module to manage the IndexedDB schemas and basic operation

// The "hidden" schema to manage change visibility state
module HiddenChangeSchema = {
  type id = string
  type t = {
    // the change id, which is also the key of the document
    id: string,
    // the date at which the change got hidden
    hidden_at: Js.Date.t,
    // a tag to enable full listing
    ctype: [#Change],
  }
  let tableName = "hidden"

  let make = (id, hidden_at) => {id: id, hidden_at: hidden_at, ctype: #Change}
}

let mkDexie: unit => Dexie.Database.t = () => {
  let dexie = Dexie.Database.make("Monocle")
  let schema = [("hidden", "&id,ctype")]
  dexie->Dexie.Database.version(1)->Dexie.Version.stores(schema)->ignore
  dexie
}
