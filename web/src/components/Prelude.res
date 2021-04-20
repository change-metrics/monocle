// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// A library for monocle web
//

// Missing bindings from re-patternfly
include Patternfly

// Bindings for existing javascript
type axiosResponse<'d> = {data: 'd}
type axios<'data> = Js.Promise.t<axiosResponse<'data>>
type axiosGet<'data> = unit => axios<'data>

@module("../api.js") external apiUrl: string = "baseurl"
@module("../api.js")
external getIndices: unit => axios<array<string>> = "getIndices"
@val @scope(("window", "location"))
external windowLocationSearch: string = "search"

// A temporary module to provide runtime setting
module Env = {
  type t = {
    @as("REACT_APP_RHBZ")
    rhbz: option<string>,
  }
  @val @scope("process") external env: t = "env"
  let withBZ = env.rhbz->Belt.Option.isSome
  let bzPriority = list{"HIGH", "MEDIUM", "LOW"}
  let bzType = list{"FutureFeature", "ZStream", "Triaged"}
}

module URLSearchParams = {
  // https://developer.mozilla.org/en-US/docs/Web/API/URLSearchParams
  type t

  @new external make: string => t = "URLSearchParams"
  @send external toString: t => string = "toString"
  @send external get: (t, string) => Js.Nullable.t<string> = "get"
  @send external set: (t, string, string) => unit = "set"
  @send external delete: (t, string) => unit = "delete"

  let current = () => windowLocationSearch->make
}

// Network helpers

// useAutoGet perform the 'get' effect when the calling component is mounted.
type result<'a, 'b> = Belt.Result.t<'a, 'b>
let useAutoGet = (get: axiosGet<'data>): option<result<'data, string>> => {
  let (state, set') = React.useState(_ => None)
  let set = x => set'(_ => x->Some)->Js.Promise.resolve
  let handleErr = err => {
    Js.log(err)
    "Network error"->Error->set
  }
  let handleOk = resp => resp.data->Ok->set
  React.useEffect0(() => {
    (get() |> Js.Promise.then_(handleOk) |> Js.Promise.catch(handleErr))->ignore
    None
  })
  state
}

// Convenient functions

let str = React.string
// Render component if the predicate is true
let maybeRender = (pred: bool, component: React.element): React.element =>
  pred ? component : React.null
// Get an optional value with default
let fromMaybe = (maybe: option<'a>, default: 'a): 'a => maybe->Belt.Option.getWithDefault(default)
let mapWithKey = (xs, f) => xs->Belt.List.mapWithIndex((index, x) => f(string_of_int(index), x))
// Remove empty optional from a list
let rec catMaybes = (xs: list<option<'a>>): list<'a> =>
  switch xs {
  | list{} => list{}
  | list{None, ...rest} => catMaybes(rest)
  | list{Some(x), ...rest} => catMaybes(rest)->Belt.List.add(x)
  }
// Join a list of string with a separator
let rec concatSep = (xs: list<string>, sep: string): string =>
  switch xs {
  | list{} => ""
  | list{x} => x
  | list{x, y, ...rest} => x ++ sep ++ y ++ rest->concatSep(sep)
  }

let useToggle = default => {
  let (value, setValue) = React.useState(_ => default)
  let toggle = () => setValue(x => !x)
  let set = x => setValue(_ => x)
  (value, toggle, set)
}

// Monocle style:
// an expandable panel
module MExpandablePanel = {
  @react.component
  let make = (~title, ~children) => {
    let (show, setShow) = React.useState(_ => false)
    let toggleProps = {
      "id": "toggle-button",
      "aria-label": "Details",
      "aria-labelledby": "titleId toggle-button",
      "aria-expanded": !show,
      "icon": React.null,
    }
    <Card isExpanded=show>
      <CardHeader onExpand={(_, _) => setShow(v => !v)} toggleButtonProps={toggleProps}>
        <CardTitle> {title->React.string} </CardTitle>
      </CardHeader>
      <CardExpandableContent> <CardBody> {children} </CardBody> </CardExpandableContent>
    </Card>
  }
}

// a grid with space between element
module MGrid = {
  @react.component
  let make = (~children) =>
    <Patternfly.Layout.Grid hasGutter=true> {children} </Patternfly.Layout.Grid>
}

// a grid element that can fit 3 on medium screen
module MGridItem = {
  @react.component
  let make = (~children) =>
    <Patternfly.Layout.GridItem md=Column._4> {children} </Patternfly.Layout.GridItem>
}

module MStack = {
  @react.component
  let make = (~children) =>
    <Patternfly.Layout.Stack hasGutter=true> {children} </Patternfly.Layout.Stack>
}

module MStackItem = {
  @react.component
  let make = (~children) => <Patternfly.Layout.StackItem> {children} </Patternfly.Layout.StackItem>
}

module MSelect = {
  @react.component
  let make = (~value: string, ~placeholder: string, ~options, ~valueChanged) => {
    let (isOpen, _, onToggle) = useToggle(false)
    let onSelect = (_, newValue, _) => {
      let nextValue = newValue == value ? "" : newValue
      valueChanged(nextValue)
      onToggle(false)
    }
    let placeholderText = placeholder
    let selections = value != "" ? [value] : []
    let inlineFilterPlaceholderText = value != "" ? placeholder ++ ": " ++ value : ""
    <Patternfly.Select
      variant=#Single
      placeholderText
      inlineFilterPlaceholderText
      selections
      isOpen
      onSelect
      onToggle>
      {options
      ->mapWithKey((key, name) => <SelectOption key value={name} />)
      ->Belt.List.toArray
      ->React.array}
    </Patternfly.Select>
  }
}

let addProp = (props, valueM, mkProp) =>
  valueM
  ->Belt.Option.flatMap(prop => prop->mkProp->Some)
  ->Belt.Option.getWithDefault(Js.Obj.empty())
  ->Js.Obj.assign(props)

// a compact card without title
module MSimpleCard = {
  @react.component
  let make = (
    ~children,
    ~style: option<ReactDOM.Style.t>=?,
    ~onClick: option<ReactEvent.Mouse.t => unit>=?,
  ) => {
    let cardProps =
      Patternfly.Card.makeProps(~isCompact=true, ())
      ->addProp(onClick, prop => {"onClick": prop})
      ->addProp(style, prop => {"style": prop})
    React.createElementVariadic(
      Patternfly.Card.make,
      cardProps,
      [
        React.createElementVariadic(
          Patternfly.CardBody.make,
          Patternfly.CardBody.makeProps(),
          [children],
        ),
      ],
    )
  }
}
