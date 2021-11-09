module HiddenChangeSchema = {
  type id = int
  type t = {
    id: string,
    hidden_at: Js.Date.t,
  }
  let tableName = "hidden"
}

module HiddenChange = Dexie.Table.MakeTable(HiddenChangeSchema)

let mkDexie: unit => Dexie.Database.t = () => {
  let dexie = Dexie.Database.make("Monocle")

  let schema = [("hidden", "++id,hidden_at")]

  dexie->Dexie.Database.version(1)->Dexie.Version.stores(schema)->ignore

  dexie
}
