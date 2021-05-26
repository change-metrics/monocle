// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// The board view
//
open Prelude

@react.component
let make = (~index: string) => {
  <MStack>
    <MStackItem> <p>{("Hey index:" ++ index)->str}</p></MStackItem>
  </MStack>
}

let default = make
