[@@@ocaml.warning "-27-30-39"]

type get_magic_jwtrequest_mutable = {
  mutable token : string;
}

let default_get_magic_jwtrequest_mutable () : get_magic_jwtrequest_mutable = {
  token = "";
}


let rec decode_get_magic_jwtrequest json =
  let v = default_get_magic_jwtrequest_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "token" -> 
      let json = Js.Dict.unsafeGet json "token" in
      v.token <- Pbrt_bs.string json "get_magic_jwtrequest" "token"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    JwtTypes.token = v.token;
  } : JwtTypes.get_magic_jwtrequest)

let rec decode_get_magic_jwterror (json:Js.Json.t) =
  match Pbrt_bs.string json "get_magic_jwterror" "value" with
  | "InvalidAdminToken" -> (JwtTypes.Invalid_admin_token : JwtTypes.get_magic_jwterror)
  | "MagicTokenDisabled" -> (JwtTypes.Magic_token_disabled : JwtTypes.get_magic_jwterror)
  | "MagicTokenCreateError" -> (JwtTypes.Magic_token_create_error : JwtTypes.get_magic_jwterror)
  | "" -> JwtTypes.Invalid_admin_token
  | _ -> Pbrt_bs.E.malformed_variant "get_magic_jwterror"

let rec decode_get_magic_jwtresponse json =
  let keys = Js.Dict.keys json in
  let rec loop = function 
    | -1 -> Pbrt_bs.E.malformed_variant "get_magic_jwtresponse"
    | i -> 
      begin match Array.unsafe_get keys i with
      | "error" -> 
        let json = Js.Dict.unsafeGet json "error" in
        (JwtTypes.Error ((decode_get_magic_jwterror json)) : JwtTypes.get_magic_jwtresponse)
      | "jwt" -> 
        let json = Js.Dict.unsafeGet json "jwt" in
        (JwtTypes.Jwt (Pbrt_bs.string json "get_magic_jwtresponse" "Jwt") : JwtTypes.get_magic_jwtresponse)
      
      | _ -> loop (i - 1)
      end
  in
  loop (Array.length keys - 1)

let rec encode_get_magic_jwtrequest (v:JwtTypes.get_magic_jwtrequest) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "token" (Js.Json.string v.JwtTypes.token);
  json

let rec encode_get_magic_jwterror (v:JwtTypes.get_magic_jwterror) : string = 
  match v with
  | JwtTypes.Invalid_admin_token -> "InvalidAdminToken"
  | JwtTypes.Magic_token_disabled -> "MagicTokenDisabled"
  | JwtTypes.Magic_token_create_error -> "MagicTokenCreateError"

let rec encode_get_magic_jwtresponse (v:JwtTypes.get_magic_jwtresponse) = 
  let json = Js.Dict.empty () in
  begin match v with
  | JwtTypes.Error v ->
    Js.Dict.set json "error" (Js.Json.string (encode_get_magic_jwterror v));
  | JwtTypes.Jwt v ->
    Js.Dict.set json "jwt" (Js.Json.string v);
  end;
  json
