// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// Render indices
//
open Prelude // 'open' bring module values into scope

module Indice = {
  @react.component
  let make = (~store, ~name) => {
    let onClick = _ => {
      store->Store.changeIndex(name)
      RescriptReactRouter.push(name)
    }
    <Tooltip position=#Bottom content={"Click to get the metric"}>
      <LegacyRouter.Link _to={"/" ++ name} onClick> {name->React.string} </LegacyRouter.Link>
    </Tooltip>
  }
  let card: (Store.t, string) => React.element = (store, name) =>
    <MSimpleCard key={name}> {make({"store": store, "name": name})} </MSimpleCard>
}

module Indices = {
  @react.component
  let make = (~store: Store.t) => {
    let indices = useAutoGet(getIndices)
    <>
      <h2> {"Available Indices"->React.string} </h2>
      <Layout.Stack>
        {switch indices {
        | None => <Spinner />
        | Some(Error(title)) => <Alert variant=#Danger title />
        | Some(Ok(indices)) if indices->Array.length > 0 =>
          indices->Belt.Array.map(Indice.card(store))->React.array
        | _ => <Alert variant=#Warning title={"Please create an index."} />
        }}
      </Layout.Stack>
    </>
  }
}

let default = Indices.make
