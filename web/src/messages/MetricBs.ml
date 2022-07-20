[@@@ocaml.warning "-27-30-39"]

type metric_info_mutable = {
  mutable name : string;
  mutable description : string;
  mutable long_description : string;
  mutable metric : string;
}

let default_metric_info_mutable () : metric_info_mutable = {
  name = "";
  description = "";
  long_description = "";
  metric = "";
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

type trend_mutable = {
  mutable interval : string;
}

let default_trend_mutable () : trend_mutable = {
  interval = "";
}

type get_request_mutable = {
  mutable index : string;
  mutable username : string;
  mutable query : string;
  mutable metric : string;
  mutable options : MetricTypes.get_request_options;
}

let default_get_request_mutable () : get_request_mutable = {
  index = "";
  username = "";
  query = "";
  metric = "";
  options = MetricTypes.Trend (MetricTypes.default_trend ());
}

type histo_int_mutable = {
  mutable date : string;
  mutable count : int32;
}

let default_histo_int_mutable () : histo_int_mutable = {
  date = "";
  count = 0l;
}

type histo_float_mutable = {
  mutable date : string;
  mutable count : float;
}

let default_histo_float_mutable () : histo_float_mutable = {
  date = "";
  count = 0.;
}

type histo_int_stat_mutable = {
  mutable histo : MetricTypes.histo_int list;
}

let default_histo_int_stat_mutable () : histo_int_stat_mutable = {
  histo = [];
}

type histo_float_stat_mutable = {
  mutable histo : MetricTypes.histo_float list;
}

let default_histo_float_stat_mutable () : histo_float_stat_mutable = {
  histo = [];
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
    | "metric" -> 
      let json = Js.Dict.unsafeGet json "metric" in
      v.metric <- Pbrt_bs.string json "metric_info" "metric"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    MetricTypes.name = v.name;
    MetricTypes.description = v.description;
    MetricTypes.long_description = v.long_description;
    MetricTypes.metric = v.metric;
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

let rec decode_trend json =
  let v = default_trend_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "interval" -> 
      let json = Js.Dict.unsafeGet json "interval" in
      v.interval <- Pbrt_bs.string json "trend" "interval"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    MetricTypes.interval = v.interval;
  } : MetricTypes.trend)

let rec decode_get_request_options json =
  let keys = Js.Dict.keys json in
  let rec loop = function 
    | -1 -> Pbrt_bs.E.malformed_variant "get_request_options"
    | i -> 
      begin match Array.unsafe_get keys i with
      | "trend" -> 
        let json = Js.Dict.unsafeGet json "trend" in
        (MetricTypes.Trend ((decode_trend (Pbrt_bs.object_ json "get_request_options" "Trend"))) : MetricTypes.get_request_options)
      
      | _ -> loop (i - 1)
      end
  in
  loop (Array.length keys - 1)

and decode_get_request json =
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
    | "trend" -> 
      let json = Js.Dict.unsafeGet json "trend" in
      v.options <- Trend ((decode_trend (Pbrt_bs.object_ json "get_request" "options")))
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    MetricTypes.index = v.index;
    MetricTypes.username = v.username;
    MetricTypes.query = v.query;
    MetricTypes.metric = v.metric;
    MetricTypes.options = v.options;
  } : MetricTypes.get_request)

let rec decode_histo_int json =
  let v = default_histo_int_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "date" -> 
      let json = Js.Dict.unsafeGet json "date" in
      v.date <- Pbrt_bs.string json "histo_int" "date"
    | "count" -> 
      let json = Js.Dict.unsafeGet json "count" in
      v.count <- Pbrt_bs.int32 json "histo_int" "count"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    MetricTypes.date = v.date;
    MetricTypes.count = v.count;
  } : MetricTypes.histo_int)

let rec decode_histo_float json =
  let v = default_histo_float_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "date" -> 
      let json = Js.Dict.unsafeGet json "date" in
      v.date <- Pbrt_bs.string json "histo_float" "date"
    | "count" -> 
      let json = Js.Dict.unsafeGet json "count" in
      v.count <- Pbrt_bs.float json "histo_float" "count"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    MetricTypes.date = v.date;
    MetricTypes.count = v.count;
  } : MetricTypes.histo_float)

let rec decode_histo_int_stat json =
  let v = default_histo_int_stat_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "histo" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "histo" in 
        Pbrt_bs.array_ a "histo_int_stat" "histo"
      in
      v.histo <- Array.map (fun json -> 
        (decode_histo_int (Pbrt_bs.object_ json "histo_int_stat" "histo"))
      ) a |> Array.to_list;
    end
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    MetricTypes.histo = v.histo;
  } : MetricTypes.histo_int_stat)

let rec decode_histo_float_stat json =
  let v = default_histo_float_stat_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "histo" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "histo" in 
        Pbrt_bs.array_ a "histo_float_stat" "histo"
      in
      v.histo <- Array.map (fun json -> 
        (decode_histo_float (Pbrt_bs.object_ json "histo_float_stat" "histo"))
      ) a |> Array.to_list;
    end
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    MetricTypes.histo = v.histo;
  } : MetricTypes.histo_float_stat)

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
      | "int_value" -> 
        let json = Js.Dict.unsafeGet json "int_value" in
        (MetricTypes.Int_value (Pbrt_bs.int32 json "get_response" "Int_value") : MetricTypes.get_response)
      | "histo_intValue" -> 
        let json = Js.Dict.unsafeGet json "histo_intValue" in
        (MetricTypes.Histo_int_value ((decode_histo_int_stat (Pbrt_bs.object_ json "get_response" "Histo_int_value"))) : MetricTypes.get_response)
      | "histo_floatValue" -> 
        let json = Js.Dict.unsafeGet json "histo_floatValue" in
        (MetricTypes.Histo_float_value ((decode_histo_float_stat (Pbrt_bs.object_ json "get_response" "Histo_float_value"))) : MetricTypes.get_response)
      
      | _ -> loop (i - 1)
      end
  in
  loop (Array.length keys - 1)

let rec encode_metric_info (v:MetricTypes.metric_info) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "name" (Js.Json.string v.MetricTypes.name);
  Js.Dict.set json "description" (Js.Json.string v.MetricTypes.description);
  Js.Dict.set json "long_description" (Js.Json.string v.MetricTypes.long_description);
  Js.Dict.set json "metric" (Js.Json.string v.MetricTypes.metric);
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

let rec encode_trend (v:MetricTypes.trend) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "interval" (Js.Json.string v.MetricTypes.interval);
  json

let rec encode_get_request_options (v:MetricTypes.get_request_options) = 
  let json = Js.Dict.empty () in
  begin match v with
  | MetricTypes.Trend v ->
    begin (* trend field *)
      let json' = encode_trend v in
      Js.Dict.set json "trend" (Js.Json.object_ json');
    end;
  end;
  json

and encode_get_request (v:MetricTypes.get_request) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "index" (Js.Json.string v.MetricTypes.index);
  Js.Dict.set json "username" (Js.Json.string v.MetricTypes.username);
  Js.Dict.set json "query" (Js.Json.string v.MetricTypes.query);
  Js.Dict.set json "metric" (Js.Json.string v.MetricTypes.metric);
  begin match v.MetricTypes.options with
    | Trend v ->
      begin (* trend field *)
        let json' = encode_trend v in
        Js.Dict.set json "trend" (Js.Json.object_ json');
      end;
  end; (* match v.options *)
  json

let rec encode_histo_int (v:MetricTypes.histo_int) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "date" (Js.Json.string v.MetricTypes.date);
  Js.Dict.set json "count" (Js.Json.number (Int32.to_float v.MetricTypes.count));
  json

let rec encode_histo_float (v:MetricTypes.histo_float) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "date" (Js.Json.string v.MetricTypes.date);
  Js.Dict.set json "count" (Js.Json.number v.MetricTypes.count);
  json

let rec encode_histo_int_stat (v:MetricTypes.histo_int_stat) = 
  let json = Js.Dict.empty () in
  begin (* histo field *)
    let (histo':Js.Json.t) =
      v.MetricTypes.histo
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_histo_int |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "histo" histo';
  end;
  json

let rec encode_histo_float_stat (v:MetricTypes.histo_float_stat) = 
  let json = Js.Dict.empty () in
  begin (* histo field *)
    let (histo':Js.Json.t) =
      v.MetricTypes.histo
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_histo_float |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "histo" histo';
  end;
  json

let rec encode_get_response (v:MetricTypes.get_response) = 
  let json = Js.Dict.empty () in
  begin match v with
  | MetricTypes.Error v ->
    Js.Dict.set json "error" (Js.Json.string v);
  | MetricTypes.Float_value v ->
    Js.Dict.set json "float_value" (Js.Json.number v);
  | MetricTypes.Int_value v ->
    Js.Dict.set json "int_value" (Js.Json.number (Int32.to_float v));
  | MetricTypes.Histo_int_value v ->
    begin (* histoIntValue field *)
      let json' = encode_histo_int_stat v in
      Js.Dict.set json "histo_intValue" (Js.Json.object_ json');
    end;
  | MetricTypes.Histo_float_value v ->
    begin (* histoFloatValue field *)
      let json' = encode_histo_float_stat v in
      Js.Dict.set json "histo_floatValue" (Js.Json.object_ json');
    end;
  end;
  json
