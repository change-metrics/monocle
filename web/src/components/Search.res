// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// The search bar component
//

open Prelude

// Change those during development
let startWithFieldModalOpen = false
let startWithOrderModalOpen = false

module FieldSelectorModal = {
  module FieldSelector = {
    @react.component
    let make = (~store: Store.t, ~fieldName, ~setFieldName, ~fieldValue, ~setFieldValue) =>
      // We fetch the suggestions once, after the modal is displayed
      switch (Store.Fetch.suggestions(store), Store.Fetch.fields(store)) {
      | (Some(Ok(suggestions)), Some(Ok(fields))) => {
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
      | _ => <Spinner />
      }
  }

  @react.component
  let make = (~isOpen, ~onClose: option<(string, string)> => unit, ~store) => {
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
        <FieldSelector store fieldName setFieldName fieldValue setFieldValue />
      </div>
    </Patternfly.Modal>
  }
}

module Bar = {
  let quoteValue = v => Js.String.includes(" ", v) ? "\"" ++ v ++ "\"" : v

  @react.component
  let make = (~store: Store.t, ~value: string, ~setValue: string => unit) => {
    let (showFieldSelector, setShowFieldSelector) = React.useState(_ => startWithFieldModalOpen)
    let appendField = v => {
      switch v {
      | Some(fieldName, fieldValues) => {
          let prefix = value == "" ? "" : " and "
          let expr = switch Js.String.split(",", fieldValues)->Belt.Array.map(fieldValue =>
            fieldName ++ ":" ++ fieldValue->quoteValue
          ) {
          | [x] => x
          | xs => "(" ++ Js.Array.joinWith(" or ", xs) ++ ")"
          }
          setValue(expr ++ prefix ++ value)
        }
      | None => ignore()
      }
      setShowFieldSelector(_ => false)
    }
    <>
      <FieldSelectorModal store isOpen={showFieldSelector} onClose={appendField} />
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

module OrderSelectorModal = {
  module OrderSelector = {
    @react.component
    let make = (
      ~store: Store.t,
      ~fieldName,
      ~setFieldName,
      ~directionValue: SearchTypes.order_direction,
      ~setDirectionValue,
    ) => {
      let directionName = switch directionValue {
      | Asc => "Ascending"
      | Desc => "Descending"
      }
      let setDirectionName = v =>
        setDirectionValue(_ =>
          switch v {
          | "Descending" => SearchTypes.Desc
          | "Ascending"
          | _ =>
            SearchTypes.Asc
          }
        )
      let sortable = (f: SearchTypes.field) => f.type_ != SearchTypes.Field_regex
      switch Store.Fetch.fields(store) {
      | Some(Ok(fields)) => <>
          <MSelect
            placeholder={"Pick a field"}
            options={fields->Belt.List.keep(sortable)->Belt.List.map(f => f.name)}
            multi={false}
            value={fieldName}
            valueChanged={v => setFieldName(_ => v)}
          />
          <MSelect
            placeholder={"Ascending"}
            options={list{"Ascending", "Descending"}}
            multi={false}
            value={directionName}
            valueChanged={v => setDirectionName(v)}
          />
        </>
      | _ => <Spinner />
      }
    }
  }
  @react.component
  let make = (
    ~isOpen,
    ~value: option<SearchTypes.order>,
    ~onClose: option<SearchTypes.order> => unit,
    ~store,
  ) => {
    let getOrder = (valueM, selector, defaultValue) =>
      valueM->Belt.Option.flatMap(v => v->selector->Some)->Belt.Option.getWithDefault(defaultValue)
    let (fieldName, setFieldName) = React.useState(_ => value->getOrder(v => v.field, ""))
    let (directionValue, setDirectionValue) = React.useState(_ =>
      value->getOrder(v => v.direction, SearchTypes.Asc)
    )
    let onConfirm = _ =>
      switch fieldName {
      | "" => None
      | _ => {field: fieldName, direction: directionValue}->Some
      }->onClose
    let onCancel = _ => None->onClose
    <Patternfly.Modal
      title="Order selector"
      variant=#Small
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
        <OrderSelector store fieldName setFieldName directionValue setDirectionValue />
      </div>
    </Patternfly.Modal>
  }
}

module Order = {
  let toStr = (dir: SearchTypes.order_direction) =>
    switch dir {
    | Asc => ""
    | Desc => " DESC"
    }
  @react.component
  let make = (
    ~store: Store.t,
    ~value: option<SearchTypes.order>,
    ~setValue: option<SearchTypes.order> => unit,
  ) => {
    let (showOrderSelector, setShowOrderSelector) = React.useState(_ => startWithOrderModalOpen)
    let setOrder = v => {
      v->setValue
      setShowOrderSelector(_ => false)
    }
    let onClick = _ => setShowOrderSelector(_ => true)
    <>
      <OrderSelectorModal store value isOpen={showOrderSelector} onClose={setOrder} />
      {switch value {
      | None => <Patternfly.Button onClick> {"Set order"->str} </Patternfly.Button>
      | Some(order) =>
        <span>
          <Patternfly.Button variant=#Tertiary onClick> {"Change Order"} </Patternfly.Button>
          {("order by " ++ order.field ++ order.direction->toStr)->str}
        </span>
      }}
    </>
  }
}

module Top = {
  @react.component
  let make = (~store: Store.t, ~withLimit: bool=false) => {
    let (state, dispatch) = store
    // The local state
    let (value, setValue') = React.useState(() => state.query)
    let (limit, setLimit') = React.useState(() => state.limit)
    let (savedValue, setSavedValue) = React.useState(() => state.query)
    let setValue = v => setValue'(_ => v)

    // Dispatch the value upstream
    let onClick = _ => {
      setSavedValue(_ => value)
      value->Store.Store.SetQuery->dispatch
    }
    let setLimit = str => {
      let v = str == "" ? 0 : str->int_of_string
      setLimit'(_ => v)
      v->Store.Store.SetLimit->dispatch
    }

    <Patternfly.Layout.Bullseye>
      <div style={ReactDOM.Style.make(~width="1024px", ~display="flex", ())}>
        <Bar value setValue store />
        {value != savedValue
          ? <Patternfly.Button _type=#Submit onClick> {"Apply"->str} </Patternfly.Button>
          : React.null}
        {withLimit
          ? <div style={ReactDOM.Style.make(~width="170px", ())}>
              <MSelect
                placeholder={"Set limit"}
                options={list{5, 10, 50, 100}->Belt.List.map(string_of_int)}
                multi={false}
                value={limit > 0 ? limit->string_of_int : ""}
                valueChanged={setLimit}
              />
            </div>
          : React.null}
      </div>
    </Patternfly.Layout.Bullseye>
  }
}
