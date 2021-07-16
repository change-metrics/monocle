// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// A library for monocle web
//

open Prelude

// Monocle nav
module MonoLink = {
  @react.component
  let make = (~store: Store.t, ~filter: string, ~path: string, ~name: string) => {
    let (state, dispatch) = store
    let onClick = _ => filter->Store.Store.SetFilter->dispatch
    <Link
      onClick
      style={ReactDOM.Style.make(~whiteSpace="nowrap", ())}
      _to={"/" ++ state.index ++ "/" ++ path ++ "?q=" ++ state.query ++ "&f=" ++ filter}>
      {name->str}
    </Link>
  }
}
