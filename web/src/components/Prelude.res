// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// A library for monocle web
//

// Missing bindings from re-patternfly
include Patternfly // 'include' export a module value to open scope
open WebApi

// axiosGetCallback<string> is a function that goes from unit to axios<string>
type axiosGetCallback<'data> = unit => axios<'data>

// See https://rescript-lang.org/docs/manual/latest/interop-cheatsheet
@module("../api.js") external apiUrl: string = "baseurl"
@module("../api.js")
external getIndices: unit => axios<array<string>> = "getIndices"
@val @scope(("window", "location"))
external windowLocationSearch: string = "search"
let readWindowLocationSearch = () => windowLocationSearch

// Bindings for moment
%%raw(`
import moment from 'moment'
`)
let momentFromNow = %raw(`
  function(d) {
    return moment(d).fromNow()
  }
`)

let resetLocationSearch = %raw(`
  function() {
    window.history.replaceState(null, null, window.location.origin + window.location.pathname);
  }
`)

let setLocationSearch = %raw(`
  function(q, v) {
    const url = new URL(window.location.href);
    url.searchParams.set(q, v);
    window.history.replaceState(null, null, url);
  }
`)

// Bindings for react router
module Link = {
  @react.component @module("react-router-dom")
  external make: (
    ~_to: string,
    ~children: 'children,
    ~style: option<'style>=?,
    ~target: option<string>=?,
    ~onClick: option<'cb>=?,
  ) => React.element = "Link"
}

// A temporary module to provide runtime setting
module Env = {
  type t = {
    @as("TD")
    td: string,
  }
  type react_env = {
    @as("REACT_APP_TD")
    td: option<string>,
  }
  @val external env: t = "window"
  @val @scope("process") external react_env: react_env = "env"
}

module Time = {
  type t

  let getNow = Js.Date.make

  let getSimpleDate = (d: Js.Date.t): string =>
    Js.String.split("T", d->Js.Date.toISOString)->Belt.Array.getUnsafe(0)

  let getDateMinusYear = (delta: int): string => {
    let now = getNow()
    let curYear = now->Js.Date.getUTCFullYear->Belt.Float.toInt
    let newYear = (curYear - delta)->Belt.Float.fromInt
    // Mutate the date
    Js.Date.setUTCFullYear(now, newYear)->ignore
    now->getSimpleDate
  }

  let getDateMinusMonth = (delta: int): string => {
    let now = getNow()
    let curMonth = now->Js.Date.getUTCMonth->Belt.Float.toInt
    let newMonth = (curMonth - delta)->Belt.Float.fromInt
    // Mutate the date
    Js.Date.setUTCMonth(now, newMonth)->ignore
    now->getSimpleDate
  }

  let getDateMinusWeek = (delta: int): string => {
    let now = getNow()
    let curHour = now->Js.Date.getUTCHours->Belt.Float.toInt
    let newHour = (curHour - delta * 24 * 7)->Belt.Float.fromInt
    // Mutate the date
    Js.Date.setUTCHours(now, newHour)->ignore
    now->getSimpleDate
  }
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
type auto<'data> = option<result<'data, string>>
let useAutoGetOn = (get: axiosGetCallback<'data>, key: string): option<result<'data, string>> => {
  let (state, set') = React.useState(_ => None)
  let set = x => set'(_ => x->Some)->Js.Promise.resolve
  let handleErr = err => {
    Js.log(err)
    "Network error"->Error->set
  }
  let handleOk = resp => resp.data->Ok->set
  React.useEffect1(() => {
    (get() |> Js.Promise.then_(handleOk) |> Js.Promise.catch(handleErr))->ignore
    None
  }, [key])
  state
}
let useAutoGet = x => x->useAutoGetOn("")

// Convenient functions

let str = React.string
// Render component if the predicate is true
let maybeRender = (pred: bool, component: React.element): React.element =>
  pred ? component : React.null
// Render component if the list is not empty
let maybeRenderArray = (xs: array<'a>, component) =>
  xs->Belt.Array.length > 0 ? component : React.null
let maybeRenderList = (xs: list<'a>, component) =>
  switch xs {
  | list{} => React.null
  | _ => component
  }
let lower = s => s->Js.String.toLowerCase
let int32_str = i32 => string_of_int(Int32.to_int(i32))

// the take from haskell prelude
let rec take: (list<'a>, int) => list<'a> = (xs, count) =>
  switch xs {
  | list{x, ...tail} if count > 0 => tail->take(count - 1)->Belt.List.add(x)
  | _ => list{}
  }

// Check if a text list contains an element
let elemText = (xs: list<string>, x: string) => xs->Belt.List.has(x, (a, b) => a == b)
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
let rec prependToAll = (xs, sep): string =>
  switch xs {
  | list{} => ""
  | list{x, ...rest} => sep ++ x ++ rest->prependToAll(sep)
  }
let concatSep = (xs: list<string>, sep: string): string =>
  switch xs {
  | list{} => ""
  | list{x, ...rest} => x ++ rest->prependToAll(sep)
  }

let useToggle = default => {
  let (value, setValue) = React.useState(_ => default)
  let toggle = () => setValue(x => !x)
  let set = x => setValue(_ => x)
  (value, toggle, set)
}

// Combine two queries
let addQuery = (baseQuery, extraQuery) => {
  let isEmpty = s => s->Js.String.trim == ""
  let (query, sep) = baseQuery->isEmpty ? ("", "") : ("(" ++ baseQuery ++ ")", " and ")
  query ++ (extraQuery->isEmpty ? "" : sep ++ extraQuery)
}

// Decode/encode order
let orderFromQS: string => option<SearchTypes.order> = queryString =>
  switch Js.String.split("#", queryString) {
  | [field, "A"] => {field: field, direction: Asc}->Some
  | [field, "D"] => {field: field, direction: Desc}->Some
  | _ => None
  }

let orderDirToQS = (dir: SearchTypes.order_direction) =>
  switch dir {
  | Asc => "#A"
  | Desc => "#D"
  }

let orderToQS = (order: option<SearchTypes.order>) =>
  switch order {
  | None => ""
  | Some({field, direction}) => field ++ direction->orderDirToQS
  }

let orderDirToString = (dir: SearchTypes.order_direction) =>
  switch dir {
  | Asc => ""
  | Desc => " DESC"
  }

// Monocle style:
// an expandable panel
module MExpandablePanel = {
  @react.component
  let make = (~title, ~children, ~stateControler: option<(bool, (bool => bool) => unit)>=?) => {
    let (show, setShow) = switch stateControler {
    | Some(s, st) => (s, st)
    | None => React.useState(_ => false)
    }
    let toggleProps = {
      "id": "toggle-button",
      "aria-label": "Details",
      "aria-labelledby": "titleId toggle-button",
      "aria-expanded": !show,
      "icon": React.null,
    }
    <Card isCompact={true} isExpanded=show>
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
  let maxCount = 20

  @react.component
  let make = (
    ~value: string,
    ~placeholder: string,
    ~multi: bool=true,
    ~isCreatable: bool=false,
    ~options,
    ~valueChanged,
  ) => {
    let (isOpen, _, onToggle) = useToggle(false)
    let selections = value != "" ? Js.String.split(",", value) : []
    let onSelect = (_, newValue, _) => {
      let nextValues = multi
        ? selections->Js.Array2.includes(newValue)
            ? selections->Js.Array2.filter(v => v != newValue)
            : selections->Js.Array2.concat([newValue])
        : [newValue]
      nextValues->Js.Array2.joinWith(",")->valueChanged
      onToggle(false)
    }
    let onClear = _ => {
      valueChanged("")
    }
    let onCreateOption = v => {
      selections->Js.Array2.concat([v])->Js.Array2.joinWith(",")->valueChanged
      onToggle(false)
    }
    let placeholderText = placeholder
    let variant = multi ? #Typeaheadmulti : #Single
    let inlineFilterPlaceholderText = value != "" ? placeholder ++ ": " ++ value : ""
    let onFilter = (_, currentInput) => {
      switch currentInput {
      | "" => options
      | _ => options->Belt.List.keep(opt => currentInput->lower->Js.String.startsWith(opt->lower))
      }
      ->take(maxCount)
      ->Belt.List.map(s => <SelectOption key={s} value={s} />)
      ->Belt.List.toArray
    }

    <Patternfly.Select
      variant
      placeholderText
      inlineFilterPlaceholderText
      selections
      isOpen
      onClear
      onSelect
      onCreateOption
      onFilter
      onToggle
      isCreatable>
      {options
      ->take(maxCount)
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
