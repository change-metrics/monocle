// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// The search help component
//
open Prelude

@react.component
let make = (~store: Store.t) => {
  let renderFieldType = (ft: SearchTypes.field_type) =>
    switch ft {
    | SearchTypes.Field_date => "date"
    | SearchTypes.Field_number => "number"
    | SearchTypes.Field_text => "text"
    | SearchTypes.Field_bool => "boolean"
    | SearchTypes.Field_regex => "regex"
    }
  let renderField = (f: SearchTypes.field) =>
    <li key={f.name}>
      <b> {f.name->str} </b>
      {(" : " ++ f.description ++ " (" ++ renderFieldType(f.type_) ++ ")")->str}
    </li>
  let li = s => <li> {s->str} </li>
  let content = switch Store.Fetch.fields(store) {
  | Some(Ok(fields)) =>
    <div style={ReactDOM.Style.make(~textAlign="left", ())}>
      <h3> {"Example"->str} </h3>
      <ul>
        {"state:open and review_count:0"->li}
        {"(repo : openstack/nova or repo: openstack/neutron) and user_group:qa and updated_at > 2020 order by score"->li}
      </ul>
      <h3> {"Available fields"->str} </h3>
      <ul> {fields->Belt.List.map(renderField)->Belt.List.toArray->React.array} </ul>
      <h3> {"Search syntax"->str} </h3>
      <ul> {"expr and expr"->li} {"expr order by field limit count"->li} </ul>
      <h3> {"Expr syntax"->str} </h3>
      <ul> {"field:value"->li} {"date_field>YYYY-MM-DD"->li} {"number_field>42"->li} </ul>
    </div>
  | _ => <Spinner />
  }
  <Patternfly.Tooltip position=#Bottom content>
    <Patternfly.Icons.OutlinedQuestionCircle />
  </Patternfly.Tooltip>
}

let default = make
