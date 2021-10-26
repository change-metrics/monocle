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
    let make = (
      ~store: Store.t,
      ~fieldName,
      ~setFieldName,
      ~fieldValue,
      ~setFieldValue,
      ~fromValue,
      ~setFromValue,
      ~toValue,
      ~setToValue,
    ) =>
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
            | "task.priority" => suggestions.priorities
            | "task.severity" => suggestions.severities
            | "task.tag" => suggestions.task_types
            | "project" => suggestions.projects
            | "group" => suggestions.groups
            | "tag" => suggestions.labels
            | _ => list{}
            }

          let value = fieldValue
          let onChange = (v, _) => setFieldValue(_ => v)
          let freeFormFields = (f: SearchTypes.field) =>
            !(list{"from", "to", "created_at", "updated_at"}->elemText(f.name))

          <>
            <FormGroup label="Date range" fieldId="date-range-form" hasNoPaddingTop=false>
              <DatePicker
                id={"from-date"}
                placeholder={"From date"}
                value={fromValue}
                onChange={(v, _) => setFromValue(_ => v)}
              />
              <DatePicker
                id={"to-date"}
                placeholder={"To date"}
                value={toValue}
                onChange={(v, _) => setToValue(_ => v)}
              />
            </FormGroup>
            <br />
            <FormGroup label="Extra filter" fieldId="query-filter" hasNoPaddingTop=false>
              <MSelect
                placeholder={"Pick a field"}
                options={fields->Belt.List.keep(freeFormFields)->Belt.List.map(f => f.name)}
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
            </FormGroup>
            <br />
            <br />
          </>
        }
      | _ => <Spinner />
      }
  }

  @react.component
  let make = (~isOpen, ~onClose: option<string> => unit, ~store) => {
    let (fieldName, setFieldName) = React.useState(_ => "")
    let (fieldValue, setFieldValue) = React.useState(_ => "")
    let (fromValue, setFromValue) = React.useState(_ => "")
    let (toValue, setToValue) = React.useState(_ => "")

    let submit = res => {
      setFromValue(_ => "")
      setToValue(_ => "")
      setFieldName(_ => "")
      setFieldValue(_ => "")
      onClose(res)
    }
    let onConfirm = _ => {
      let field =
        fieldValue == ""
          ? ""
          : switch Js.String.split(",", fieldValue)->Belt.Array.map(fieldValue =>
              fieldName ++ ":" ++ fieldValue->quoteValue
            ) {
            | [x] => x
            | xs => "(" ++ Js.Array.joinWith(" or ", xs) ++ ")"
            }
      let setExpr = (value, field) => value == "" ? "" : field ++ ":" ++ value
      let fromExpr = fromValue->setExpr("from")
      let toExpr = toValue->setExpr("to")
      let expr = list{field, fromExpr, toExpr}->Belt.List.keep(v => v != "")->concatSep(" ")
      expr->Some->submit
    }
    let onCancel = _ => None->submit
    <Patternfly.Modal
      title="Add search filter"
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
        <FieldSelector
          store
          fieldName
          setFieldName
          fieldValue
          setFieldValue
          fromValue
          setFromValue
          toValue
          setToValue
        />
      </div>
    </Patternfly.Modal>
  }
}

// A custom text input with onKeyUp callback
// TODO: add property to re-patternfly
module TextInputUp = {
  @react.component @module("@patternfly/react-core")
  external make: (
    ~iconVariant: @string
    [
      | @as("search") #Search
    ]=?,
    ~validated: @string
    [
      | @as("warning") #Error
    ]=?,
    ~id: string,
    ~onChange: (string, ReactEvent.Mouse.t) => unit=?,
    ~onKeyUp: ReactEvent.Keyboard.t => unit=?,
    ~_type: @string
    [
      | @as("text") #Text
    ]=?,
    ~value: string=?,
  ) => React.element = "TextInput"
}

module Bar = {
  @react.component
  let make = (
    ~store: Store.t,
    ~value: string,
    ~setValue: string => unit,
    ~onSave: string => unit,
    ~showTooltips: bool=true,
    ~error: option<string>,
  ) => {
    let (showFieldSelector, setShowFieldSelector) = React.useState(_ => startWithFieldModalOpen)
    let appendExpr = expr => {
      switch expr {
      | Some(expr) => {
          let prefix = expr == "" ? "" : " "
          onSave(expr ++ prefix ++ value)
        }
      | None => ignore()
      }
      setShowFieldSelector(_ => false)
    }
    let onKeyUp = (v: ReactEvent.Keyboard.t) =>
      switch ReactEvent.Keyboard.key(v) {
      | "Enter" => onSave(value)
      | _ => ignore()
      }
    let id = "col-search"
    let onChange = (v, _) => setValue(v)
    let _type = #Text
    <MGrid>
      <MGridItemXl2>
        <FieldSelectorModal store isOpen={showFieldSelector} onClose={appendExpr} />
        {showTooltips ? <HelpSearch.Tooltip store /> : React.null}
        <Patternfly.Button onClick={_ => setShowFieldSelector(_ => true)}>
          {"Add filter"->str}
        </Patternfly.Button>
      </MGridItemXl2>
      <MGridItemXl10>
        {
          // Patternfly TextInput doesn't render properly when both validated and iconVariant are set.
          // Instead of doing complicated dynamic props, this switch duplicate the common props
          switch error {
          | Some(_) => <TextInputUp id value onChange onKeyUp _type validated=#Error />
          | None => <TextInputUp id value onChange onKeyUp _type iconVariant=#Search />
          }
        }
      </MGridItemXl10>
    </MGrid>
  }
}

module OrderSelectorModal = {
  module OrderSelector = {
    @react.component
    let make = (
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
      let fields = list{"created_at", "updated_at", "repo", "score"}
      <>
        <MSelect
          placeholder={"Pick a field"}
          options={fields}
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
    }
  }
  @react.component
  let make = (
    ~isOpen,
    ~value: option<SearchTypes.order>,
    ~onClose: option<SearchTypes.order> => unit,
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
        <OrderSelector fieldName setFieldName directionValue setDirectionValue />
      </div>
    </Patternfly.Modal>
  }
}

module Order = {
  @react.component
  let make = (~value: option<SearchTypes.order>, ~setValue: option<SearchTypes.order> => unit) => {
    let (showOrderSelector, setShowOrderSelector) = React.useState(_ => startWithOrderModalOpen)
    let setOrder = v => {
      v->setValue
      setShowOrderSelector(_ => false)
    }
    let onClick = _ => setShowOrderSelector(_ => true)
    <>
      <OrderSelectorModal value isOpen={showOrderSelector} onClose={setOrder} />
      {switch value {
      | None => <Patternfly.Button onClick> {"Set order"->str} </Patternfly.Button>
      | Some(order) =>
        <span>
          <Patternfly.Button variant=#Tertiary onClick> {"Change Order"} </Patternfly.Button>
          {("order by " ++ order.field ++ order.direction->orderDirToString)->str}
        </span>
      }}
    </>
  }
}

let formatError = (query, message, position) => {
  let from = position->Int32.to_int
  // When the error position starts at 0, no need to include the context since it is
  // already visible in the search bar
  message ++ (from == 0 ? "" : " (" ++ Js.String.sliceToEnd(~from, query) ++ ")")
}

module Top = {
  @react.component
  let make = (~store: Store.t, ~withLimit: bool=false) => {
    let (state, dispatch) = store
    // The local state
    let (value, setValue') = React.useState(() => state.query)
    let (limit, setLimit') = React.useState(() => state.limit)
    let (savedValue, setSavedValue) = React.useState(() => state.query)
    let (error, setError) = React.useState(() => None)
    let setValue = v => setValue'(_ => v)

    // Update changed value
    React.useEffect1(() => {
      state.query != value
        ? {
            setValue'(_ => state.query)
            setSavedValue(_ => state.query)
          }
        : ignore()
      None
    }, [state.query])

    // Dispatch the value upstream
    let handleCheck = (newValue, res: WebApi.axiosResponse<SearchTypes.check_response>) => {
      switch res.data {
      | SearchTypes.Success(_) => {
          setError(_ => None)
          setSavedValue(_ => newValue)
          newValue->Store.Store.SetQuery->dispatch
        }
      | Error({message, position}) => setError(_ => formatError(newValue, message, position)->Some)
      }->Js.Promise.resolve
    }
    let onSave = newValue => {
      (WebApi.Search.check({
        index: state.index,
        username: "",
        query: value,
      }) |> Js.Promise.then_(handleCheck(newValue)))->ignore
    }
    let onClick = _ => onSave(value)

    let setLimit = str => {
      let v = str == "" ? 0 : str->int_of_string
      setLimit'(_ => v)
      v->Store.Store.SetLimit->dispatch
    }

    <Patternfly.Layout.Bullseye>
      <div style={ReactDOM.Style.make(~width="1024px", ~display="flex", ())}>
        <Bar value setValue onSave store error />
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
      {
        // Not the prettiest thing, but this gets the job done
        switch error {
        | Some(title) => <> <br /> <Alert variant=#Danger title /> </>
        | None => React.null
        }
      }
    </Patternfly.Layout.Bullseye>
  }
}

module Filter = {
  @react.component
  let make = (~store: Store.t) => {
    let (state, dispatch) = store
    let (order, setOrder) = React.useState(_ => state.order)
    let setValue = v => {
      v->Store.Store.SetOrder->dispatch
      setOrder(_ => v)
    }
    <div style={ReactDOM.Style.make(~width="1024px", ~whiteSpace="nowrap", ())}>
      <Order value={order} setValue />
      {state.filter == ""
        ? React.null
        : <>
            <Button onClick={_ => ""->Store.Store.SetFilter->dispatch}>
              {"Clear Filter"->str}
            </Button>
            <Patternfly.TextInput
              id="col-filter" value={state.filter} _type=#Text iconVariant=#Search isDisabled={true}
            />
          </>}
    </div>
  }
}
