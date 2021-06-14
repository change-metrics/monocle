// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// The monocle auth helpers
//

type t = {username: string}

@module("../api.js") external whoAmIRaw: (t => unit, unit => unit) => unit = "getAuth"

let whoAmI = () => {
  let authOk = resp => Js.log2("RESP", resp)
  let needAuth = _ => Js.log("Need auth!")
  whoAmIRaw(authOk, needAuth)
}
