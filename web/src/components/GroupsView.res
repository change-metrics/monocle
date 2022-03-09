open Prelude

module GroupItem = {
  @react.component
  let make = (~store: Store.t, ~group: ConfigTypes.group_definition) => {
    let (state, _) = store
    let link = "/" ++ state.index ++ "/group/" ++ group.name
    <MSimpleCard
      style={ReactDOM.Style.make(~cursor="pointer", ())}
      onClick={_ => link->RescriptReactRouter.push}>
      {(group.name ++ " (" ++ group.members->Int32.to_int->string_of_int ++ " member)")->str}
    </MSimpleCard>
  }
}

@react.component
let make = (~store: Store.t) => {
  let (state, _) = store
  let title = "User Groups"
  let tooltip_content = "This shows the list of available user groups"
  let icon = <Patternfly.Icons.Users />
  let trigger = ""

  let toItems = (resp: ConfigTypes.get_groups_response) => {
    resp.items->Belt.List.map(group => <GroupItem store key={group.name} group />)
  }

  <MonoCard title tooltip_content icon>
    <NetworkRender
      get={() => WebApi.Config.getGroups({ConfigTypes.index: state.index})}
      trigger
      render={resp => resp->toItems->Belt.List.toArray->React.array}
    />
  </MonoCard>
}

let default = make
