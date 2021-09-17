open Prelude

module GroupItem = {
  @react.component
  let make = (~store: Store.t, ~group: UserGroupTypes.group_definition) => {
    let (state, _dispatch) = store
    <MSimpleCard>
      <MLink.Direct
        link={"/" ++ state.index ++ "/user_groups/" ++ group.name}
        name={group.name ++ " (" ++ group.members->Int32.to_int->string_of_int ++ " member)"}
      />
    </MSimpleCard>
  }
}

@react.component
let make = (~store: Store.t) => {
  let title = "User Groups"
  let tooltip_content = "This shows the list of available user groups"
  let icon = <Patternfly.Icons.Users />
  <MCenteredContent>
    <MonoCard title tooltip_content icon>
      {switch Store.Fetch.user_groups(store) {
      | None => <Spinner />
      | Some(Error(title)) => <Alert variant=#Danger title />
      | Some(Ok({items: list{}})) => <Alert variant=#Warning title={"Please define user groups."} />
      | Some(Ok({items})) =>
        items
        ->Belt.List.map(group => <GroupItem store key={group.name} group />)
        ->Belt.List.toArray
        ->React.array
      }}
    </MonoCard>
  </MCenteredContent>
}

let default = make
