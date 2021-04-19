// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// A library for monocle web
//

// Missing bindings from re-patternfly
include Patternfly
include Layout

module Tooltip = {
  @react.component @module("@patternfly/react-core")
  external make: (
    ~children: 'children,
    ~position: @string
    [
      | @as("top") #Top
      | @as("bottom") #Bottom
    ]=?,
    ~content: string=?,
  ) => React.element = "Tooltip"
}

// Bindings for existing javascript
type axiosResponse<'d> = {data: 'd}
type axios<'data> = Js.Promise.t<axiosResponse<'data>>
type axiosGet<'data> = unit => axios<'data>

@module("../api.js") external apiUrl: string = "baseurl"
@module("../api.js")
external getIndices: unit => axios<array<string>> = "getIndices"

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
let maybeRender = (pred, component) => pred ? component : React.null

// Monocle style:
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
