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

type get_request_mutable = {
  mutable index : string;
  mutable username : string;
  mutable query : string;
  mutable metric : string;
}

let default_get_request_mutable () : get_request_mutable = {
  index = "";
  username = "";
  query = "";
  metric = "";
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

let rec decode_get_request json =
  let v = default_get_request_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "index" -> 
      let json = Js.Dict.unsafeGet json "index" in
      v.index <- Pbrt_bs.string json "get_request" "index"
    | "username" -> 
      let json = Js.Dict.unsafeGet json "username" in
      v.username <- Pbrt_bs.string json "get_request" "username"
    | "query" -> 
      let json = Js.Dict.unsafeGet json "query" in
      v.query <- Pbrt_bs.string json "get_request" "query"
    | "metric" -> 
      let json = Js.Dict.unsafeGet json "metric" in
      v.metric <- Pbrt_bs.string json "get_request" "metric"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    MetricTypes.index = v.index;
    MetricTypes.username = v.username;
    MetricTypes.query = v.query;
    MetricTypes.metric = v.metric;
  } : MetricTypes.get_request)

let rec decode_get_response json =
  let keys = Js.Dict.keys json in
  let rec loop = function 
    | -1 -> Pbrt_bs.E.malformed_variant "get_response"
    | i -> 
      begin match Array.unsafe_get keys i with
      | "error" -> 
        let json = Js.Dict.unsafeGet json "error" in
        (MetricTypes.Error (Pbrt_bs.string json "get_response" "Error") : MetricTypes.get_response)
      | "float_value" -> 
        let json = Js.Dict.unsafeGet json "float_value" in
        (MetricTypes.Float_value (Pbrt_bs.float json "get_response" "Float_value") : MetricTypes.get_response)
      
      | _ -> loop (i - 1)
      end
  in
  loop (Array.length keys - 1)

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

let rec encode_get_request (v:MetricTypes.get_request) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "index" (Js.Json.string v.MetricTypes.index);
  Js.Dict.set json "username" (Js.Json.string v.MetricTypes.username);
  Js.Dict.set json "query" (Js.Json.string v.MetricTypes.query);
  Js.Dict.set json "metric" (Js.Json.string v.MetricTypes.metric);
  json

let rec encode_get_response (v:MetricTypes.get_response) = 
  let json = Js.Dict.empty () in
  begin match v with
  | MetricTypes.Error v ->
    Js.Dict.set json "error" (Js.Json.string v);
  | MetricTypes.Float_value v ->
    Js.Dict.set json "float_value" (Js.Json.number v);
  end;
  json
