[@@@ocaml.warning "-27-30-39"]

type get_magic_jwt_request_mutable = {
  mutable token : string;
}

let default_get_magic_jwt_request_mutable () : get_magic_jwt_request_mutable = {
  token = "";
}

type who_ami_request_mutable = {
  mutable void : string;
}

let default_who_ami_request_mutable () : who_ami_request_mutable = {
  void = "";
}


let rec decode_get_magic_jwt_request json =
  let v = default_get_magic_jwt_request_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "token" -> 
      let json = Js.Dict.unsafeGet json "token" in
      v.token <- Pbrt_bs.string json "get_magic_jwt_request" "token"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    AuthTypes.token = v.token;
  } : AuthTypes.get_magic_jwt_request)

let rec decode_get_magic_jwt_error (json:Js.Json.t) =
  match Pbrt_bs.string json "get_magic_jwt_error" "value" with
  | "InvalidAdminToken" -> (AuthTypes.Invalid_admin_token : AuthTypes.get_magic_jwt_error)
  | "MagicTokenDisabled" -> (AuthTypes.Magic_token_disabled : AuthTypes.get_magic_jwt_error)
  | "MagicTokenCreateError" -> (AuthTypes.Magic_token_create_error : AuthTypes.get_magic_jwt_error)
  | "" -> AuthTypes.Invalid_admin_token
  | _ -> Pbrt_bs.E.malformed_variant "get_magic_jwt_error"

let rec decode_get_magic_jwt_response json =
  let keys = Js.Dict.keys json in
  let rec loop = function 
    | -1 -> Pbrt_bs.E.malformed_variant "get_magic_jwt_response"
    | i -> 
      begin match Array.unsafe_get keys i with
      | "error" -> 
        let json = Js.Dict.unsafeGet json "error" in
        (AuthTypes.Error ((decode_get_magic_jwt_error json)) : AuthTypes.get_magic_jwt_response)
      | "jwt" -> 
        let json = Js.Dict.unsafeGet json "jwt" in
        (AuthTypes.Jwt (Pbrt_bs.string json "get_magic_jwt_response" "Jwt") : AuthTypes.get_magic_jwt_response)
      
      | _ -> loop (i - 1)
      end
  in
  loop (Array.length keys - 1)

let rec decode_who_ami_request json =
  let v = default_who_ami_request_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "void" -> 
      let json = Js.Dict.unsafeGet json "void" in
      v.void <- Pbrt_bs.string json "who_ami_request" "void"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    AuthTypes.void = v.void;
  } : AuthTypes.who_ami_request)

let rec decode_who_ami_error (json:Js.Json.t) =
  match Pbrt_bs.string json "who_ami_error" "value" with
  | "UnAuthorized" -> (AuthTypes.Un_authorized : AuthTypes.who_ami_error)
  | "" -> AuthTypes.Un_authorized
  | _ -> Pbrt_bs.E.malformed_variant "who_ami_error"

let rec decode_who_ami_response json =
  let keys = Js.Dict.keys json in
  let rec loop = function 
    | -1 -> Pbrt_bs.E.malformed_variant "who_ami_response"
    | i -> 
      begin match Array.unsafe_get keys i with
      | "error" -> 
        let json = Js.Dict.unsafeGet json "error" in
        (AuthTypes.Error ((decode_who_ami_error json)) : AuthTypes.who_ami_response)
      | "uid" -> 
        let json = Js.Dict.unsafeGet json "uid" in
        (AuthTypes.Uid (Pbrt_bs.string json "who_ami_response" "Uid") : AuthTypes.who_ami_response)
      
      | _ -> loop (i - 1)
      end
  in
  loop (Array.length keys - 1)

let rec encode_get_magic_jwt_request (v:AuthTypes.get_magic_jwt_request) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "token" (Js.Json.string v.AuthTypes.token);
  json

let rec encode_get_magic_jwt_error (v:AuthTypes.get_magic_jwt_error) : string = 
  match v with
  | AuthTypes.Invalid_admin_token -> "InvalidAdminToken"
  | AuthTypes.Magic_token_disabled -> "MagicTokenDisabled"
  | AuthTypes.Magic_token_create_error -> "MagicTokenCreateError"

let rec encode_get_magic_jwt_response (v:AuthTypes.get_magic_jwt_response) = 
  let json = Js.Dict.empty () in
  begin match v with
  | AuthTypes.Error v ->
    Js.Dict.set json "error" (Js.Json.string (encode_get_magic_jwt_error v));
  | AuthTypes.Jwt v ->
    Js.Dict.set json "jwt" (Js.Json.string v);
  end;
  json

let rec encode_who_ami_request (v:AuthTypes.who_ami_request) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "void" (Js.Json.string v.AuthTypes.void);
  json

let rec encode_who_ami_error (v:AuthTypes.who_ami_error) : string = 
  match v with
  | AuthTypes.Un_authorized -> "UnAuthorized"

let rec encode_who_ami_response (v:AuthTypes.who_ami_response) = 
  let json = Js.Dict.empty () in
  begin match v with
  | AuthTypes.Error v ->
    Js.Dict.set json "error" (Js.Json.string (encode_who_ami_error v));
  | AuthTypes.Uid v ->
    Js.Dict.set json "uid" (Js.Json.string v);
  end;
  json
