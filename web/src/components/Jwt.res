// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// Some JSON Web Token utils for Monocle
//

%%raw(`
import jwt_decode from "jwt-decode";
`)
let jwtDecode = %raw(`
  function(token) {
    return jwt_decode(token)
  }
`)

type authenticatedUser = {
  jwt: string,
  jwt_exp: Js.Date.t,
  uid: string,
}

let monocleJwtDecode = jwt => {
  try {
    let decodedJwt = jwtDecode(jwt)
    let exp = decodedJwt["exp"]->Belt.Float.fromString->Belt.Option.getExn
    {
      jwt: jwt,
      jwt_exp: (exp *. 1000.0)->Js.Date.fromFloat,
      uid: decodedJwt["dat"]["aMuid"],
    }->Some
  } catch {
  | err => {
      Js.log2("Unable to decode Monocle JWT due to:", err)
      None
    }
  }
}
