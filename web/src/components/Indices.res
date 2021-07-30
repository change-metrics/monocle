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
    }
    <Tooltip position=#Bottom content={"Click to get the metric"}>
      <a onClick> {name->React.string} </a>
    </Tooltip>
  }
  let card: (Store.t, ConfigTypes.workspace) => React.element = (store, ws) =>
    <MSimpleCard key={ws.name}> {make({"store": store, "name": ws.name})} </MSimpleCard>
}

module Indices = {
  @react.component
  let make = (~store: Store.t) => {
    let indices = useAutoGet(() => WebApi.Config.getWorkspaces({void: ""}))
    <>
      <h2> {"Available Workspaces"->React.string} </h2>
      <Layout.Stack>
        {switch indices {
        | None => <Spinner />
        | Some(Error(title)) => <Alert variant=#Danger title />
        | Some(Ok({workspaces})) if workspaces->Belt.List.length > 0 =>
          workspaces->Belt.List.map(Indice.card(store))->Belt.List.toArray->React.array
        | _ => <Alert variant=#Warning title={"Please create a workspace."} />
        }}
      </Layout.Stack>
    </>
  }
}

let default = Indices.make
