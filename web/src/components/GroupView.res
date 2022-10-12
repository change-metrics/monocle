// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// The user group view component
//

open Prelude

module UserItem = {
  @react.component
  let make = (~store: Store.t, ~name: string) => {
    let (state, dispatch) = store
    let link = "/" ++ state.index ++ "/author/" ++ name->Js.Global.encodeURIComponent
    <MSimpleCard
      style={ReactDOM.Style.make(~cursor="pointer", ())}
      onClick={e => {
        e->ReactEvent.Mouse.preventDefault
        ChangeActivity->SetAuthorScopedTab->dispatch
        link->RescriptReactRouter.push
      }}>
      {name->str}
    </MSimpleCard>
  }
}

@react.component
let make = (~group: string, ~store: Store.t) => {
  let (state, _) = store
  let title = "User Group members"
  let tooltip_content = "This shows the list of group's members"
  let icon = <Patternfly.Icons.Users />
  let trigger = ""

  let toItems = (resp: ConfigTypes.get_group_members_response) => {
    resp.members->Belt.List.map(name => <UserItem store key={name} name />)
  }

  <MonoCard title tooltip_content icon>
    <NetworkRender
      get={() =>
        WebApi.Config.getGroupMembers({ConfigTypes.index: state.index, ConfigTypes.group: group})}
      trigger
      render={resp => resp->toItems->Belt.List.toArray->React.array}
    />
  </MonoCard>
}

let default = make
