[@@@ocaml.warning "-27-30-39"]

type who_am_irequest_mutable = {
  mutable noop : string;
}

let default_who_am_irequest_mutable () : who_am_irequest_mutable = {
  noop = "";
}

type who_am_iresponse_mutable = {
  mutable userid : string;
}

let default_who_am_iresponse_mutable () : who_am_iresponse_mutable = {
  userid = "";
}


let rec decode_who_am_irequest json =
  let v = default_who_am_irequest_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "noop" -> 
      let json = Js.Dict.unsafeGet json "noop" in
      v.noop <- Pbrt_bs.string json "who_am_irequest" "noop"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    AuthTypes.noop = v.noop;
  } : AuthTypes.who_am_irequest)

let rec decode_who_am_iresponse json =
  let v = default_who_am_iresponse_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "userid" -> 
      let json = Js.Dict.unsafeGet json "userid" in
      v.userid <- Pbrt_bs.string json "who_am_iresponse" "userid"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    AuthTypes.userid = v.userid;
  } : AuthTypes.who_am_iresponse)

let rec encode_who_am_irequest (v:AuthTypes.who_am_irequest) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "noop" (Js.Json.string v.AuthTypes.noop);
  json

let rec encode_who_am_iresponse (v:AuthTypes.who_am_iresponse) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "userid" (Js.Json.string v.AuthTypes.userid);
  json
