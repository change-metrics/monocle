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
    <a onClick> {name->React.string} </a>
  }
  let card: (Store.t, ConfigTypes.workspace) => React.element = (store, ws) =>
    <MSimpleCard key={ws.name}> {make({"store": store, "name": ws.name})} </MSimpleCard>
}

module Indices = {
  @react.component
  let make = (~store: Store.t) => {
    let (state, _) = store
    let title = "Workspaces"
    let tooltip_content = "This shows the list of available workspaces"
    let icon = <Patternfly.Icons.Bundle />
    let tokenM = state->Store.Store.getAuthenticatedUserJWT
    <MCenteredContent>
      <MonoCard title tooltip_content icon>
        <NetworkRender
          get={() => WebApi.Config.getWorkspaces({void: ""}, tokenM)}
          trigger={""}
          render={(resp: ConfigTypes.get_workspaces_response) =>
            resp.workspaces->Belt.List.length > 0
              ? resp.workspaces->Belt.List.map(Indice.card(store))->Belt.List.toArray->React.array
              : <Alert variant=#Warning title={"Please create a workspace."} />}
        />
      </MonoCard>
    </MCenteredContent>
  }
}

let default = Indices.make
