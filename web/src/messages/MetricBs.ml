[@@@ocaml.warning "-27-30-39"]

type metric_info_mutable = {
  mutable name : string;
  mutable description : string;
  mutable long_description : string;
}

let default_metric_info_mutable () : metric_info_mutable = {
  name = "";
  description = "";
  long_description = "";
}

type list_request_mutable = {
  mutable void : string;
}

let default_list_request_mutable () : list_request_mutable = {
  void = "";
}

type list_response_mutable = {
  mutable metrics : MetricTypes.metric_info list;
}

let default_list_response_mutable () : list_response_mutable = {
  metrics = [];
}


let rec decode_metric_info json =
  let v = default_metric_info_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "name" -> 
      let json = Js.Dict.unsafeGet json "name" in
      v.name <- Pbrt_bs.string json "metric_info" "name"
    | "description" -> 
      let json = Js.Dict.unsafeGet json "description" in
      v.description <- Pbrt_bs.string json "metric_info" "description"
    | "long_description" -> 
      let json = Js.Dict.unsafeGet json "long_description" in
      v.long_description <- Pbrt_bs.string json "metric_info" "long_description"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    MetricTypes.name = v.name;
    MetricTypes.description = v.description;
    MetricTypes.long_description = v.long_description;
  } : MetricTypes.metric_info)

let rec decode_list_request json =
  let v = default_list_request_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "void" -> 
      let json = Js.Dict.unsafeGet json "void" in
      v.void <- Pbrt_bs.string json "list_request" "void"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    MetricTypes.void = v.void;
  } : MetricTypes.list_request)

let rec decode_list_response json =
  let v = default_list_response_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "metrics" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "metrics" in 
        Pbrt_bs.array_ a "list_response" "metrics"
      in
      v.metrics <- Array.map (fun json -> 
        (decode_metric_info (Pbrt_bs.object_ json "list_response" "metrics"))
      ) a |> Array.to_list;
    end
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    MetricTypes.metrics = v.metrics;
  } : MetricTypes.list_response)

let rec encode_metric_info (v:MetricTypes.metric_info) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "name" (Js.Json.string v.MetricTypes.name);
  Js.Dict.set json "description" (Js.Json.string v.MetricTypes.description);
  Js.Dict.set json "long_description" (Js.Json.string v.MetricTypes.long_description);
  json

let rec encode_list_request (v:MetricTypes.list_request) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "void" (Js.Json.string v.MetricTypes.void);
  json

let rec encode_list_response (v:MetricTypes.list_response) = 
  let json = Js.Dict.empty () in
  begin (* metrics field *)
    let (metrics':Js.Json.t) =
      v.MetricTypes.metrics
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_metric_info |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "metrics" metrics';
  end;
  json
