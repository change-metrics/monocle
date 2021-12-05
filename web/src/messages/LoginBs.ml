[@@@ocaml.warning "-27-30-39"]

type login_validation_request_mutable = {
  mutable username : string;
}

let default_login_validation_request_mutable () : login_validation_request_mutable = {
  username = "";
}


let rec decode_login_validation_request json =
  let v = default_login_validation_request_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "username" -> 
      let json = Js.Dict.unsafeGet json "username" in
      v.username <- Pbrt_bs.string json "login_validation_request" "username"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    LoginTypes.username = v.username;
  } : LoginTypes.login_validation_request)

let rec decode_login_validation_response_validation_result (json:Js.Json.t) =
  match Pbrt_bs.string json "login_validation_response_validation_result" "value" with
  | "UnknownIdent" -> (LoginTypes.Unknown_ident : LoginTypes.login_validation_response_validation_result)
  | "KnownIdent" -> (LoginTypes.Known_ident : LoginTypes.login_validation_response_validation_result)
  | "" -> LoginTypes.Unknown_ident
  | _ -> Pbrt_bs.E.malformed_variant "login_validation_response_validation_result"

let rec decode_login_validation_response json =
  let keys = Js.Dict.keys json in
  let rec loop = function 
    | -1 -> Pbrt_bs.E.malformed_variant "login_validation_response"
    | i -> 
      begin match Array.unsafe_get keys i with
      | "validation_result" -> 
        let json = Js.Dict.unsafeGet json "validation_result" in
        (LoginTypes.Validation_result ((decode_login_validation_response_validation_result json)) : LoginTypes.login_validation_response)
      
      | _ -> loop (i - 1)
      end
  in
  loop (Array.length keys - 1)

let rec encode_login_validation_request (v:LoginTypes.login_validation_request) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "username" (Js.Json.string v.LoginTypes.username);
  json

let rec encode_login_validation_response_validation_result (v:LoginTypes.login_validation_response_validation_result) : string = 
  match v with
  | LoginTypes.Unknown_ident -> "UnknownIdent"
  | LoginTypes.Known_ident -> "KnownIdent"

let rec encode_login_validation_response (v:LoginTypes.login_validation_response) = 
  let json = Js.Dict.empty () in
  begin match v with
  | LoginTypes.Validation_result v ->
    Js.Dict.set json "validation_result" (Js.Json.string (encode_login_validation_response_validation_result v));
  end;
  json
