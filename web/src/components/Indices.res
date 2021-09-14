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
  let make = (~store: Store.t) => <>
    <h2> {"Available Workspaces"->React.string} </h2>
    <Layout.Stack>
      <NetworkRender
        get={() => WebApi.Config.getWorkspaces({void: ""})}
        trigger={""}
        render={(resp: ConfigTypes.get_workspaces_response) =>
          resp.workspaces->Belt.List.length > 0
            ? resp.workspaces->Belt.List.map(Indice.card(store))->Belt.List.toArray->React.array
            : <Alert variant=#Warning title={"Please create a workspace."} />}
      />
    </Layout.Stack>
  </>
}

let default = Indices.make
