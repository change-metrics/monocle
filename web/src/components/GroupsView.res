open Prelude

module GroupItem = {
  @react.component
  let make = (~store: Store.t, ~group: UserGroupTypes.group_definition) => {
    let (state, dispatch) = store
    let onClick = _ => {
      RescriptReactRouter.push("/" ++ state.index ++ "/user_groups/" ++ group.name)
    }
    <MSimpleCard>
      <a href="" onClick>
        {(group.name ++ " (" ++ group.members->Int32.to_int->string_of_int ++ " member)")->str}
      </a>
    </MSimpleCard>
  }
}

@react.component
let make = (~store: Store.t) => {
  <div className="container">
    <h2> {"Available User Groups"->str} </h2>
    <Layout.Stack>
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
    </Layout.Stack>
  </div>
}

let default = make
