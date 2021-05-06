[@@@ocaml.warning "-27-30-39"]

type timestamp_mutable = {
  mutable seconds : int64;
  mutable nanos : int32;
}

let default_timestamp_mutable () : timestamp_mutable = {
  seconds = 0L;
  nanos = 0l;
}


let rec decode_timestamp json =
  let v = default_timestamp_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "seconds" -> 
      let json = Js.Dict.unsafeGet json "seconds" in
      v.seconds <- Pbrt_bs.int64 json "timestamp" "seconds"
    | "nanos" -> 
      let json = Js.Dict.unsafeGet json "nanos" in
      v.nanos <- Pbrt_bs.int32 json "timestamp" "nanos"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    TimestampTypes.seconds = v.seconds;
    TimestampTypes.nanos = v.nanos;
  } : TimestampTypes.timestamp)

let rec encode_timestamp (v:TimestampTypes.timestamp) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "seconds" (Js.Json.string (Int64.to_string v.TimestampTypes.seconds));
  Js.Dict.set json "nanos" (Js.Json.number (Int32.to_float v.TimestampTypes.nanos));
  json
