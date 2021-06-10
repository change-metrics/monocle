// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// A designer app to help style the components without the backend
//

%%raw(`
import '@patternfly/react-core/dist/styles/base.css'
import './index.css'
import 'bootstrap/dist/css/bootstrap.min.css'
import './App.css'
`)

open Prelude

module App = {
  @react.component
  let make = () => <>
    {[("title", <h2> {"Monocle designer mode"->str} </h2>), ("test", <p> {"Hi o/"->str} </p>)]
    ->Belt.Array.map((key, v) => <span key> {v} <hr /> </span>)
    ->React.array}
  </>
}

switch ReactDOM.querySelector("#root") {
| Some(root) => ReactDOM.render(<App />, root)
| None => Js.log("Can't find #root element!")
}
