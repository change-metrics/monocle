[@@@ocaml.warning "-27-30-39"]

type get_magic_jwtrequest_mutable = {
  mutable token : string;
}

let default_get_magic_jwtrequest_mutable () : get_magic_jwtrequest_mutable = {
  token = "";
}

type unauthorized_mutable = {
  mutable reason : string;
}

let default_unauthorized_mutable () : unauthorized_mutable = {
  reason = "";
}

type success_jwt_mutable = {
  mutable jwt : string;
}

let default_success_jwt_mutable () : success_jwt_mutable = {
  jwt = "";
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

let rec decode_unauthorized json =
  let v = default_unauthorized_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "reason" -> 
      let json = Js.Dict.unsafeGet json "reason" in
      v.reason <- Pbrt_bs.string json "unauthorized" "reason"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    JwtTypes.reason = v.reason;
  } : JwtTypes.unauthorized)

let rec decode_success_jwt json =
  let v = default_success_jwt_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "jwt" -> 
      let json = Js.Dict.unsafeGet json "jwt" in
      v.jwt <- Pbrt_bs.string json "success_jwt" "jwt"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    JwtTypes.jwt = v.jwt;
  } : JwtTypes.success_jwt)

let rec decode_get_magic_jwtresponse json =
  let keys = Js.Dict.keys json in
  let rec loop = function 
    | -1 -> Pbrt_bs.E.malformed_variant "get_magic_jwtresponse"
    | i -> 
      begin match Array.unsafe_get keys i with
      | "reason" -> 
        let json = Js.Dict.unsafeGet json "reason" in
        (JwtTypes.Reason ((decode_unauthorized (Pbrt_bs.object_ json "get_magic_jwtresponse" "Reason"))) : JwtTypes.get_magic_jwtresponse)
      | "success_jwt" -> 
        let json = Js.Dict.unsafeGet json "success_jwt" in
        (JwtTypes.Success_jwt ((decode_success_jwt (Pbrt_bs.object_ json "get_magic_jwtresponse" "Success_jwt"))) : JwtTypes.get_magic_jwtresponse)
      
      | _ -> loop (i - 1)
      end
  in
  loop (Array.length keys - 1)

let rec encode_get_magic_jwtrequest (v:JwtTypes.get_magic_jwtrequest) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "token" (Js.Json.string v.JwtTypes.token);
  json

let rec encode_unauthorized (v:JwtTypes.unauthorized) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "reason" (Js.Json.string v.JwtTypes.reason);
  json

let rec encode_success_jwt (v:JwtTypes.success_jwt) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "jwt" (Js.Json.string v.JwtTypes.jwt);
  json

let rec encode_get_magic_jwtresponse (v:JwtTypes.get_magic_jwtresponse) = 
  let json = Js.Dict.empty () in
  begin match v with
  | JwtTypes.Reason v ->
    begin (* reason field *)
      let json' = encode_unauthorized v in
      Js.Dict.set json "reason" (Js.Json.object_ json');
    end;
  | JwtTypes.Success_jwt v ->
    begin (* successJwt field *)
      let json' = encode_success_jwt v in
      Js.Dict.set json "success_jwt" (Js.Json.object_ json');
    end;
  end;
  json
