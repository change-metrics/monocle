// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// The search bar component
//
open Prelude

@react.component
let make = (~value: string, ~fields: list<SearchTypes.field>, ~onChange, ~onSearch) => {
  // Debounce update
  React.useEffect1(() => {
    let handler = Js.Global.setTimeout(() => onSearch(value), 500)
    Some(() => Js.Global.clearTimeout(handler))
  }, [value])

  // Help
  let renderField = (f: SearchTypes.field) =>
    <li key={f.name}> <b> {f.name->str} </b> {(" : " ++ f.description)->str} </li>
  let content =
    <div style={ReactDOM.Style.make(~textAlign="left", ())}>
      <h3> {"Search syntax"->str} </h3>
      <ul> <li> {"expr and expr"->str} </li> <li> {"expr order by field"->str} </li> </ul>
      <h3> {"Search fields"->str} </h3>
      <ul> {fields->Belt.List.map(renderField)->Belt.List.toArray->React.array} </ul>
    </div>

  <span style={ReactDOM.Style.make(~display="flex", ())}>
    <Patternfly.TextInput id="fri-search" value _type=#Text iconVariant=#Search onChange />
    <span>
      <Patternfly.Tooltip position=#Bottom content>
        <Patternfly.Icons.OutlinedQuestionCircle />
      </Patternfly.Tooltip>
    </span>
  </span>
}

let default = make
