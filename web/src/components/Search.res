// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// The search bar component
//

open Prelude

let startWithFieldModalOpen = false

module FieldSelectorModal = {
  type res<'data> = option<result<'data, string>>
  module FieldSelector = {
    @react.component
    let make = (
      ~suggestions: SearchTypes.search_suggestions_response,
      ~fields: list<SearchTypes.field>,
      ~fieldName,
      ~setFieldName,
      ~fieldValue,
      ~setFieldValue,
    ) => {
      let get: string => option<SearchTypes.field> = name =>
        fields->Belt.List.getBy(field => field.name == name)
      let getValues: string => list<string> = name =>
        switch name {
        | "state" => list{"open", "merged", "self_merged", "abandoned"}
        | "author" => suggestions.authors
        | "approval" => suggestions.approvals
        | "priority" => suggestions.priorities
        | "severity" => suggestions.severities
        | _ => list{}
        }

      let value = fieldValue
      let onChange = (v, _) => setFieldValue(_ => v)

      <>
        <MSelect
          placeholder={"Pick a field"}
          options={fields->Belt.List.map(f => f.name)}
          multi={false}
          value={fieldName}
          valueChanged={v => setFieldName(_ => v)}
        />
        {switch get(fieldName) {
        | Some(field) =>
          switch getValues(fieldName) {
          | list{} =>
            <TextInput id={"field-input"} placeholder={field.description} onChange value />
          | xs =>
            <MSelect
              isCreatable={true}
              placeholder={field.description}
              options={xs}
              valueChanged={v => setFieldValue(_ => v)}
              value
            />
          }
        | None => React.null
        }}
        <br />
        <br />
      </>
    }
  }
  @react.component
  let make = (
    ~isOpen,
    ~onClose: option<(string, string)> => unit,
    ~suggestions: res<SearchTypes.search_suggestions_response>,
    ~fields,
  ) => {
    let (fieldName, setFieldName) = React.useState(_ => "")
    let (fieldValue, setFieldValue) = React.useState(_ => "")
    let onConfirm = _ => onClose(Some(fieldName, fieldValue))
    let onCancel = _ => onClose(None)
    <Patternfly.Modal
      title="Field selector"
      variant=#Large
      isOpen
      onClose={_ => onClose(None)}
      actions=[
        <Patternfly.Button key="confirm" variant=#Primary onClick={onConfirm}>
          {"Confirm"->str}
        </Patternfly.Button>,
        <Patternfly.Button key="cancel" variant=#Link onClick={onCancel}>
          {"Cancel"->str}
        </Patternfly.Button>,
      ]>
      <div style={ReactDOM.Style.make(~height="400px", ())}>
        {switch suggestions {
        | Some(Ok(suggestions)) =>
          <FieldSelector suggestions fields fieldName setFieldName fieldValue setFieldValue />
        | _ => <Spinner />
        }}
      </div>
    </Patternfly.Modal>
  }
}

module Bar = {
  let quoteValue = v => Js.String.includes(" ", v) ? "\"" ++ v ++ "\"" : v

  @react.component
  let make = (~value: string, ~setValue: string => unit, ~fields, ~suggestions) => {
    let (showFieldSelector, setShowFieldSelector) = React.useState(_ => startWithFieldModalOpen)
    let appendField = v => {
      switch v {
      | Some(name, value) => {
          let prefix = value == "" ? "" : " and "
          let expr = switch Js.String.split(",", value)->Belt.Array.map(value =>
            name ++ ":" ++ value->quoteValue
          ) {
          | [x] => x
          | xs => "(" ++ Js.Array.joinWith(" or ", xs) ++ ")"
          }
          setValue(value ++ prefix ++ expr)
        }
      | None => ignore()
      }
      setShowFieldSelector(_ => false)
    }
    <>
      <FieldSelectorModal isOpen={showFieldSelector} onClose={appendField} fields suggestions />
      <Patternfly.Button onClick={_ => setShowFieldSelector(_ => true)}>
        {"Add field"->str}
      </Patternfly.Button>
      <Patternfly.TextInput
        id="col-search"
        value={value}
        onChange={(v, _) => setValue(v)}
        _type=#Text
        iconVariant=#Search
      />
    </>
  }
}
