// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// A designer app to help style the components without the backend
//

%%raw(`
import '@patternfly/react-core/dist/styles/base.css'
import '@patternfly/react-styles/css/components/Table/table.css'
import './index.css'
`)

open Prelude

module Fixture = {
  @module external changeJson: Js.Dict.t<Js.Json.t> = "../../schemas/monocle/change.json"
  let change: SearchTypes.change = SearchBs.decode_change(changeJson)

  @module
  external searchFieldsJson: Js.Dict.t<Js.Json.t> = "../../schemas/monocle/search_fields.json"
  let fields: SearchTypes.fields_response = SearchBs.decode_fields_response(searchFieldsJson)
}

let dispatchChange = (c => Js.log2("Hidding", c), c => Js.log2("Revealing", c))

let status = HiddenChanges.Visible

module App = {
  @react.component
  let make = () => {
    let fakeAbout: ConfigTypes.about = {
      version: "1.2.3",
      links: list{},
      auth: ConfigTypes.Auth_config(ConfigTypes.default_about_auth_config()),
    }
    let store = Store.use("test", fakeAbout)
    <>
      {[
        ("title", <h2> {"Monocle designer mode"->str} </h2>),
        (
          "change",
          <div className="container">
            <Change.DataItem store change={Fixture.change} status dispatchChange />
          </div>,
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
            store
            changes={Belt.List.make(100, Fixture.change)
            ->Belt.List.map(c => (status, c))
            ->Belt.List.toArray}
            dispatchChange
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
