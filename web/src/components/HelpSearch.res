// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// The search help component
//
open Prelude

module Content = {
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
      <b> {f.name->str} </b> {(" (" ++ renderFieldType(f.type_) ++ "): " ++ f.description)->str}
    </li>

  let li = s => <li> {s->str} </li>

  module FakeInput = {
    @react.component
    let make = (~value: string) =>
      <li> <Patternfly.TextInput id="fake" _type=#Text iconVariant=#Search value /> </li>
  }

  module Code = {
    @react.component
    let make = (~value: string) =>
      <span
        style={ReactDOM.Style.make(
          ~background="white",
          ~paddingLeft="3px",
          ~paddingRight="3px",
          ~marginLeft="2px",
          ~marginRight="2px",
          (),
        )}>
        {value->str}
      </span>
  }

  @react.component
  let make = (~fields: list<SearchTypes.field>) =>
    <div style={ReactDOM.Style.make(~textAlign="left", ())}>
      <h2> {"Monocle search language"->str} </h2>
      <p>
        {"Use the query syntax to filter the metrics. Learn more in the "->str}
        <a
          href="https://github.com/change-metrics/monocle/blob/master/doc/query-language.md#monocle-query-language">
          {"specification"->str}
        </a>
        {"."->str}
      </p>
      <p> {"Use the `Add Field` button to get suggestions."->str} </p>
      <h4> {"Example"->str} </h4>
      <ul>
        <FakeInput value="state:open approved task.score>42" />
        <FakeInput
          value="(repo:openstack/nova or repo:openstack/neutron) and group:qa and updated_at>2020"
        />
        <FakeInput value="author:\"Foo B.*\" and not state:open" />
        <FakeInput value="author:self and not state:open" />
      </ul>
      <h4> {"Syntax"->str} </h4>
      <p>
        {"Queries are defined using expressions such as "->str}
        <Code value="state:open" />
        {". Expressions can be combined using boolean operators: "->str}
        <Code value="and" />
        {", "->str}
        <Code value="not" />
        {" and "->str}
        <Code value="or" />
        {". The priority can be set with parenthesis. The boolean operator can be omitted, then it defaults to "->str}
        <Code value="and" />
        {"."->str}
      </p>
      <p>
        {"An expression is composed of a field name, a match operator and a value. For text or regex type, use the: "->str}
        <Code value=":" />
        {" match operator. For date or numeric type, use one of: "->str}
        <Code value=">" />
        <Code value=">=" />
        <Code value="<" />
        <Code value="<=" />
        {". Date value can be absolute, in the form of "->str}
        <Code value="YYYY" />
        <Code value="YYYY-MM" />
        <Code value="YYYY-MM-DD" />
        {", or relative using this syntax: "->str}
        <Code value="now-Xdays" />
        <Code value="now-Xweeks" />
        {"."->str}
      </p>
      <p>
        {"An expression can also be defined using aliases."->str}
        {"In the example above, "->str}
        <Code value="approved" />
        {" is an alias that can be defined as a compound expression for a given project."->str}
      </p>
      <h4> {"Fields"->str} </h4>
      <ul> {fields->Belt.List.map(renderField)->Belt.List.toArray->React.array} </ul>
      <h4> {"Aliases"->str} </h4>
      <ul> {"TBD"->li} </ul>
    </div>
}

module View = {
  @react.component
  let make = (~store: Store.t) =>
    <div className="container">
      {switch Store.Fetch.fields(store) {
      | Some(Ok(fields)) => <Content fields />
      | _ => <Spinner />
      }}
    </div>
}

module HelpLink = {
  let href = "/help/search"

  @react.component
  let make = (~onClose) => {
    let onClick = e => {
      e->ReactEvent.Mouse.preventDefault
      href->RescriptReactRouter.push
      onClose()
    }

    <a
      href
      onClick
      style={ReactDOM.Style.make(~paddingRight="5px", ~paddingLeft="5px", ())}
      target="_blank">
      {`🔗`->str}
    </a>
  }
}

module SearchHelpModal = {
  @react.component
  let make = (~isOpen, ~onClose, ~store: Store.t) =>
    <Patternfly.Modal
      title="Search help" variant=#Large isOpen onClose header={<HelpLink onClose />}>
      <View store />
    </Patternfly.Modal>
}

module Tooltip = {
  @react.component
  let make = (~store: Store.t) => {
    let (showHelpModal, setShowHelpModal) = React.useState(_ => false)
    let content = "Show query language syntax"
    <>
      <SearchHelpModal store isOpen={showHelpModal} onClose={_ => setShowHelpModal(_ => false)} />
      <a
        onClick={_ => setShowHelpModal(_ => true)}
        style={ReactDOM.Style.make(~color="#007bff", ())}>
        <Tooltip content> <Patternfly.Icons.OutlinedQuestionCircle /> </Tooltip>
      </a>
    </>
  }
}

let default = View.make
