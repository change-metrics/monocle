[@@@ocaml.warning "-27-30-39"]

type get_magic_jwtrequest_mutable = {
  mutable token : string;
}

let default_get_magic_jwtrequest_mutable () : get_magic_jwtrequest_mutable = {
  token = "";
}

type who_am_irequest_mutable = {
  mutable void : string;
}

let default_who_am_irequest_mutable () : who_am_irequest_mutable = {
  void = "";
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
    AuthTypes.token = v.token;
  } : AuthTypes.get_magic_jwtrequest)

let rec decode_get_magic_jwterror (json:Js.Json.t) =
  match Pbrt_bs.string json "get_magic_jwterror" "value" with
  | "InvalidAdminToken" -> (AuthTypes.Invalid_admin_token : AuthTypes.get_magic_jwterror)
  | "MagicTokenDisabled" -> (AuthTypes.Magic_token_disabled : AuthTypes.get_magic_jwterror)
  | "MagicTokenCreateError" -> (AuthTypes.Magic_token_create_error : AuthTypes.get_magic_jwterror)
  | "" -> AuthTypes.Invalid_admin_token
  | _ -> Pbrt_bs.E.malformed_variant "get_magic_jwterror"

let rec decode_get_magic_jwtresponse json =
  let keys = Js.Dict.keys json in
  let rec loop = function 
    | -1 -> Pbrt_bs.E.malformed_variant "get_magic_jwtresponse"
    | i -> 
      begin match Array.unsafe_get keys i with
      | "error" -> 
        let json = Js.Dict.unsafeGet json "error" in
        (AuthTypes.Error ((decode_get_magic_jwterror json)) : AuthTypes.get_magic_jwtresponse)
      | "jwt" -> 
        let json = Js.Dict.unsafeGet json "jwt" in
        (AuthTypes.Jwt (Pbrt_bs.string json "get_magic_jwtresponse" "Jwt") : AuthTypes.get_magic_jwtresponse)
      
      | _ -> loop (i - 1)
      end
  in
  loop (Array.length keys - 1)

let rec decode_who_am_irequest json =
  let v = default_who_am_irequest_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "void" -> 
      let json = Js.Dict.unsafeGet json "void" in
      v.void <- Pbrt_bs.string json "who_am_irequest" "void"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    AuthTypes.void = v.void;
  } : AuthTypes.who_am_irequest)

let rec decode_who_am_ierror (json:Js.Json.t) =
  match Pbrt_bs.string json "who_am_ierror" "value" with
  | "UnAuthorized" -> (AuthTypes.Un_authorized : AuthTypes.who_am_ierror)
  | "" -> AuthTypes.Un_authorized
  | _ -> Pbrt_bs.E.malformed_variant "who_am_ierror"

let rec decode_who_am_iresponse json =
  let keys = Js.Dict.keys json in
  let rec loop = function 
    | -1 -> Pbrt_bs.E.malformed_variant "who_am_iresponse"
    | i -> 
      begin match Array.unsafe_get keys i with
      | "error" -> 
        let json = Js.Dict.unsafeGet json "error" in
        (AuthTypes.Error ((decode_who_am_ierror json)) : AuthTypes.who_am_iresponse)
      | "uid" -> 
        let json = Js.Dict.unsafeGet json "uid" in
        (AuthTypes.Uid (Pbrt_bs.string json "who_am_iresponse" "Uid") : AuthTypes.who_am_iresponse)
      
      | _ -> loop (i - 1)
      end
  in
  loop (Array.length keys - 1)

let rec encode_get_magic_jwtrequest (v:AuthTypes.get_magic_jwtrequest) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "token" (Js.Json.string v.AuthTypes.token);
  json

let rec encode_get_magic_jwterror (v:AuthTypes.get_magic_jwterror) : string = 
  match v with
  | AuthTypes.Invalid_admin_token -> "InvalidAdminToken"
  | AuthTypes.Magic_token_disabled -> "MagicTokenDisabled"
  | AuthTypes.Magic_token_create_error -> "MagicTokenCreateError"

let rec encode_get_magic_jwtresponse (v:AuthTypes.get_magic_jwtresponse) = 
  let json = Js.Dict.empty () in
  begin match v with
  | AuthTypes.Error v ->
    Js.Dict.set json "error" (Js.Json.string (encode_get_magic_jwterror v));
  | AuthTypes.Jwt v ->
    Js.Dict.set json "jwt" (Js.Json.string v);
  end;
  json

let rec encode_who_am_irequest (v:AuthTypes.who_am_irequest) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "void" (Js.Json.string v.AuthTypes.void);
  json

let rec encode_who_am_ierror (v:AuthTypes.who_am_ierror) : string = 
  match v with
  | AuthTypes.Un_authorized -> "UnAuthorized"

let rec encode_who_am_iresponse (v:AuthTypes.who_am_iresponse) = 
  let json = Js.Dict.empty () in
  begin match v with
  | AuthTypes.Error v ->
    Js.Dict.set json "error" (Js.Json.string (encode_who_am_ierror v));
  | AuthTypes.Uid v ->
    Js.Dict.set json "uid" (Js.Json.string v);
  end;
  json
