// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// The user group view component
//

open Prelude

module RowItem = {
  module Head = {
    @react.component
    let make = () =>
      <thead>
        <tr role="row">
          <th role="columnheader"> {"Member"->str} </th>
          <th role="columnheader"> {"Ratio"->str} </th>
        </tr>
      </thead>
  }
  @react.component
  let make = (~user: UserGroupTypes.user_stat) => {
    Js.log2("HERE", user.stat)
    <tr role="row">
      <td role="cell"> {user.name->str} </td>
      <td role="cell">
        {(user.stat->Belt.Option.getExn).change_review_ratio->Js.Float.toString->str}
      </td>
    </tr>
  }
}

module GroupTable = {
  @react.component
  let make = (~group: UserGroupTypes.get_response) => {
    <table className="pf-c-table pf-m-compact pf-m-grid-md" role="grid">
      <RowItem.Head />
      <tbody role="rowgroup">
        {group.users
        ->Belt.List.mapWithIndex((idx, user) => <RowItem key={string_of_int(idx)} user />)
        ->Belt.List.toArray
        ->React.array}
      </tbody>
    </table>
  }
}

@react.component
let make = (~group: string, ~store: Store.t) => {
  let (state, _) = store
  let request: UserGroupTypes.get_request = {index: state.index, name: group, query: state.query}
  <MStack>
    <MStackItem> <Search.Top store /> </MStackItem>
    <MStackItem>
      <h3> {group->str} </h3>
      {switch useAutoGet(() => WebApi.UserGroup.get(request)) {
      | None => <Spinner />
      | Some(Error(title)) => <Alert variant=#Danger title />
      | Some(Ok(group)) => <GroupTable group />
      }}
    </MStackItem>
  </MStack>
}
let default = make
