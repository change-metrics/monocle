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
import './App.css'
`)

open Prelude

module Fixture = {
  @module external changeJson: Js.Dict.t<Js.Json.t> = "../../protos/monocle/change.json"
  let change: SearchTypes.change = SearchBs.decode_change(changeJson)
}

module App = {
  @react.component
  let make = () => <>
    {[
      ("title", <h2> {"Monocle designer mode"->str} </h2>),
      (
        "change",
        <div className="container">
          <Change.DataItem index={"test"} change={Fixture.change} />
        </div>,
      ),
      ("table", <Change.Table index={"test"} changes={list{Fixture.change, Fixture.change}} />),
    ]
    ->Belt.Array.map(((key, v)) => <span key> {v} <hr /> </span>)
    ->React.array}
  </>
}

switch ReactDOM.querySelector("#root") {
| Some(root) => ReactDOM.render(<App />, root)
| None => Js.log("Can't find #root element!")
}
