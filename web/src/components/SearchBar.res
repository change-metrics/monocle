// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// The search bar component
//
open Prelude

@react.component
let make = (~value: string, ~onChange, ~onSearch) => {
  // Debounce update
  React.useEffect1(() => {
    let handler = Js.Global.setTimeout(() => onSearch(value), 500)
    Some(() => Js.Global.clearTimeout(handler))
  }, [value])

  <Patternfly.TextInput id="fri-search" value _type=#Text iconVariant=#Search onChange />
}

let default = make
