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
  defaultMuid: string,
  muidMap: Js.Dict.t<string>,
}

let jwtToAuthenticatedUser = jwt => {
  try {
    let decodedJwt = jwtDecode(jwt)
    let exp = decodedJwt["exp"]->Belt.Float.fromString->Belt.Option.getExn
    {
      jwt: jwt,
      jwt_exp: (exp *. 1000.0)->Js.Date.fromFloat,
      defaultMuid: decodedJwt["dat"]["aDefaultMuid"],
      muidMap: decodedJwt["dat"]["aMuidMap"],
    }->Some
  } catch {
  | err => {
      Js.log2("Unable to decode Monocle JWT due to:", err)
      None
    }
  }
}

let getMuidByIndex = (au: authenticatedUser, index: string) => au.muidMap->Js.Dict.get(index)
