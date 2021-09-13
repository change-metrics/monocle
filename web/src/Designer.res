// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// A designer app to help style the components without the backend
//

%%raw(`
import '@patternfly/react-core/dist/styles/base.css'
import '@patternfly/react-styles/css/components/Table/table.css'
import './index.css'
import 'bootstrap/dist/css/bootstrap.min.css'
`)

open Prelude

module Fixture = {
  @module external changeJson: Js.Dict.t<Js.Json.t> = "../../protos/monocle/change.json"
  let change: SearchTypes.change = SearchBs.decode_change(changeJson)

  @module external groupGetJson: Js.Dict.t<Js.Json.t> = "../../protos/monocle/user_group_get.json"
  let group_get: UserGroupTypes.get_response = UserGroupBs.decode_get_response(groupGetJson)

  @module
  external searchFieldsJson: Js.Dict.t<Js.Json.t> = "../../protos/monocle/search_fields.json"
  let fields: SearchTypes.fields_response = SearchBs.decode_fields_response(searchFieldsJson)
}

module App = {
  @react.component
  let make = () => {
    let store = Store.use("test")
    <>
      {[
        ("title", <h2> {"Monocle designer mode"->str} </h2>),
        ("group", <GroupView.GroupTable group={Fixture.group_get} />),
        (
          "change",
          <div className="container"> <Change.DataItem store change={Fixture.change} /> </div>,
        ),
        (
          "table",
          <Change.Table
            store
            changes={Belt.List.make(200, Fixture.change)->Belt.List.mapWithIndex((idx, change) => {
              ...change,
              title: "test " ++ string_of_int(idx),
            })}
          />,
        ),
        (
          "changeList",
          <NChangeView.ChangeList
            store changes={Belt.List.make(100, Fixture.change)->Belt.List.toArray}
          />,
        ),
        (
          "search help",
          <>
            <div className="container"> <HelpSearch.Tooltip store /> {"test"->str} </div>
            <div className="container"> <HelpSearch.Content fields={Fixture.fields.fields} /> </div>
          </>,
        ),
      ]
      ->Belt.Array.map(((key, v)) => <span key> {v} <hr /> </span>)
      ->React.array}
    </>
  }
}

switch ReactDOM.querySelector("#root") {
| Some(root) => ReactDOM.render(<App />, root)
| None => Js.log("Can't find #root element!")
}
