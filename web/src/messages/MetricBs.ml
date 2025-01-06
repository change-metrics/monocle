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

type top_mutable = {
  mutable limit : int32;
}

let default_top_mutable () : top_mutable = {
  limit = 0l;
}

type compute_mutable = {
  mutable void : string;
}

let default_compute_mutable () : compute_mutable = {
  void = "";
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
  options = MetricTypes.Compute (MetricTypes.default_compute ());
}

type info_request_mutable = {
  mutable metric : string;
}

let default_info_request_mutable () : info_request_mutable = {
  metric = "";
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

type term_count_int_mutable = {
  mutable term : string;
  mutable count : int32;
}

let default_term_count_int_mutable () : term_count_int_mutable = {
  term = "";
  count = 0l;
}

type terms_count_int_mutable = {
  mutable termcount : MetricTypes.term_count_int list;
  mutable total_hits : int32;
}

let default_terms_count_int_mutable () : terms_count_int_mutable = {
  termcount = [];
  total_hits = 0l;
}

type term_count_float_mutable = {
  mutable term : string;
  mutable count : float;
}

let default_term_count_float_mutable () : term_count_float_mutable = {
  term = "";
  count = 0.;
}

type terms_count_float_mutable = {
  mutable termcount : MetricTypes.term_count_float list;
  mutable total_hits : int32;
}

let default_terms_count_float_mutable () : terms_count_float_mutable = {
  termcount = [];
  total_hits = 0l;
}

type duration_mutable = {
  mutable value : int32;
}

let default_duration_mutable () : duration_mutable = {
  value = 0l;
}

type histo_duration_mutable = {
  mutable date : string;
  mutable count : int32;
}

let default_histo_duration_mutable () : histo_duration_mutable = {
  date = "";
  count = 0l;
}

type histo_duration_stat_mutable = {
  mutable histo : MetricTypes.histo_duration list;
}

let default_histo_duration_stat_mutable () : histo_duration_stat_mutable = {
  histo = [];
}

type term_count_duration_mutable = {
  mutable term : string;
  mutable count : int32;
}

let default_term_count_duration_mutable () : term_count_duration_mutable = {
  term = "";
  count = 0l;
}

type terms_count_duration_mutable = {
  mutable termcount : MetricTypes.term_count_duration list;
  mutable total_hits : int32;
}

let default_terms_count_duration_mutable () : terms_count_duration_mutable = {
  termcount = [];
  total_hits = 0l;
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

let rec decode_top json =
  let v = default_top_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "limit" -> 
      let json = Js.Dict.unsafeGet json "limit" in
      v.limit <- Pbrt_bs.int32 json "top" "limit"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    MetricTypes.limit = v.limit;
  } : MetricTypes.top)

let rec decode_compute json =
  let v = default_compute_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "void" -> 
      let json = Js.Dict.unsafeGet json "void" in
      v.void <- Pbrt_bs.string json "compute" "void"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    MetricTypes.void = v.void;
  } : MetricTypes.compute)

let rec decode_get_request_options json =
  let keys = Js.Dict.keys json in
  let rec loop = function 
    | -1 -> Pbrt_bs.E.malformed_variant "get_request_options"
    | i -> 
      begin match Array.unsafe_get keys i with
      | "compute" -> 
        let json = Js.Dict.unsafeGet json "compute" in
        (MetricTypes.Compute ((decode_compute (Pbrt_bs.object_ json "get_request_options" "Compute"))) : MetricTypes.get_request_options)
      | "trend" -> 
        let json = Js.Dict.unsafeGet json "trend" in
        (MetricTypes.Trend ((decode_trend (Pbrt_bs.object_ json "get_request_options" "Trend"))) : MetricTypes.get_request_options)
      | "top" -> 
        let json = Js.Dict.unsafeGet json "top" in
        (MetricTypes.Top ((decode_top (Pbrt_bs.object_ json "get_request_options" "Top"))) : MetricTypes.get_request_options)
      
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
    | "compute" -> 
      let json = Js.Dict.unsafeGet json "compute" in
      v.options <- Compute ((decode_compute (Pbrt_bs.object_ json "get_request" "options")))
    | "trend" -> 
      let json = Js.Dict.unsafeGet json "trend" in
      v.options <- Trend ((decode_trend (Pbrt_bs.object_ json "get_request" "options")))
    | "top" -> 
      let json = Js.Dict.unsafeGet json "top" in
      v.options <- Top ((decode_top (Pbrt_bs.object_ json "get_request" "options")))
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    MetricTypes.index = v.index;
    MetricTypes.username = v.username;
    MetricTypes.query = v.query;
    MetricTypes.metric = v.metric;
    MetricTypes.options = v.options;
  } : MetricTypes.get_request)

let rec decode_info_request json =
  let v = default_info_request_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "metric" -> 
      let json = Js.Dict.unsafeGet json "metric" in
      v.metric <- Pbrt_bs.string json "info_request" "metric"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    MetricTypes.metric = v.metric;
  } : MetricTypes.info_request)

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

let rec decode_term_count_int json =
  let v = default_term_count_int_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "term" -> 
      let json = Js.Dict.unsafeGet json "term" in
      v.term <- Pbrt_bs.string json "term_count_int" "term"
    | "count" -> 
      let json = Js.Dict.unsafeGet json "count" in
      v.count <- Pbrt_bs.int32 json "term_count_int" "count"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    MetricTypes.term = v.term;
    MetricTypes.count = v.count;
  } : MetricTypes.term_count_int)

let rec decode_terms_count_int json =
  let v = default_terms_count_int_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "termcount" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "termcount" in 
        Pbrt_bs.array_ a "terms_count_int" "termcount"
      in
      v.termcount <- Array.map (fun json -> 
        (decode_term_count_int (Pbrt_bs.object_ json "terms_count_int" "termcount"))
      ) a |> Array.to_list;
    end
    | "total_hits" -> 
      let json = Js.Dict.unsafeGet json "total_hits" in
      v.total_hits <- Pbrt_bs.int32 json "terms_count_int" "total_hits"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    MetricTypes.termcount = v.termcount;
    MetricTypes.total_hits = v.total_hits;
  } : MetricTypes.terms_count_int)

let rec decode_term_count_float json =
  let v = default_term_count_float_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "term" -> 
      let json = Js.Dict.unsafeGet json "term" in
      v.term <- Pbrt_bs.string json "term_count_float" "term"
    | "count" -> 
      let json = Js.Dict.unsafeGet json "count" in
      v.count <- Pbrt_bs.float json "term_count_float" "count"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    MetricTypes.term = v.term;
    MetricTypes.count = v.count;
  } : MetricTypes.term_count_float)

let rec decode_terms_count_float json =
  let v = default_terms_count_float_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "termcount" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "termcount" in 
        Pbrt_bs.array_ a "terms_count_float" "termcount"
      in
      v.termcount <- Array.map (fun json -> 
        (decode_term_count_float (Pbrt_bs.object_ json "terms_count_float" "termcount"))
      ) a |> Array.to_list;
    end
    | "total_hits" -> 
      let json = Js.Dict.unsafeGet json "total_hits" in
      v.total_hits <- Pbrt_bs.int32 json "terms_count_float" "total_hits"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    MetricTypes.termcount = v.termcount;
    MetricTypes.total_hits = v.total_hits;
  } : MetricTypes.terms_count_float)

let rec decode_duration json =
  let v = default_duration_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "value" -> 
      let json = Js.Dict.unsafeGet json "value" in
      v.value <- Pbrt_bs.int32 json "duration" "value"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    MetricTypes.value = v.value;
  } : MetricTypes.duration)

let rec decode_histo_duration json =
  let v = default_histo_duration_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "date" -> 
      let json = Js.Dict.unsafeGet json "date" in
      v.date <- Pbrt_bs.string json "histo_duration" "date"
    | "count" -> 
      let json = Js.Dict.unsafeGet json "count" in
      v.count <- Pbrt_bs.int32 json "histo_duration" "count"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    MetricTypes.date = v.date;
    MetricTypes.count = v.count;
  } : MetricTypes.histo_duration)

let rec decode_histo_duration_stat json =
  let v = default_histo_duration_stat_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "histo" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "histo" in 
        Pbrt_bs.array_ a "histo_duration_stat" "histo"
      in
      v.histo <- Array.map (fun json -> 
        (decode_histo_duration (Pbrt_bs.object_ json "histo_duration_stat" "histo"))
      ) a |> Array.to_list;
    end
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    MetricTypes.histo = v.histo;
  } : MetricTypes.histo_duration_stat)

let rec decode_term_count_duration json =
  let v = default_term_count_duration_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "term" -> 
      let json = Js.Dict.unsafeGet json "term" in
      v.term <- Pbrt_bs.string json "term_count_duration" "term"
    | "count" -> 
      let json = Js.Dict.unsafeGet json "count" in
      v.count <- Pbrt_bs.int32 json "term_count_duration" "count"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    MetricTypes.term = v.term;
    MetricTypes.count = v.count;
  } : MetricTypes.term_count_duration)

let rec decode_terms_count_duration json =
  let v = default_terms_count_duration_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "termcount" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "termcount" in 
        Pbrt_bs.array_ a "terms_count_duration" "termcount"
      in
      v.termcount <- Array.map (fun json -> 
        (decode_term_count_duration (Pbrt_bs.object_ json "terms_count_duration" "termcount"))
      ) a |> Array.to_list;
    end
    | "total_hits" -> 
      let json = Js.Dict.unsafeGet json "total_hits" in
      v.total_hits <- Pbrt_bs.int32 json "terms_count_duration" "total_hits"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    MetricTypes.termcount = v.termcount;
    MetricTypes.total_hits = v.total_hits;
  } : MetricTypes.terms_count_duration)

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
      | "histo_int" -> 
        let json = Js.Dict.unsafeGet json "histo_int" in
        (MetricTypes.Histo_int ((decode_histo_int_stat (Pbrt_bs.object_ json "get_response" "Histo_int"))) : MetricTypes.get_response)
      | "histo_float" -> 
        let json = Js.Dict.unsafeGet json "histo_float" in
        (MetricTypes.Histo_float ((decode_histo_float_stat (Pbrt_bs.object_ json "get_response" "Histo_float"))) : MetricTypes.get_response)
      | "top_int" -> 
        let json = Js.Dict.unsafeGet json "top_int" in
        (MetricTypes.Top_int ((decode_terms_count_int (Pbrt_bs.object_ json "get_response" "Top_int"))) : MetricTypes.get_response)
      | "top_float" -> 
        let json = Js.Dict.unsafeGet json "top_float" in
        (MetricTypes.Top_float ((decode_terms_count_float (Pbrt_bs.object_ json "get_response" "Top_float"))) : MetricTypes.get_response)
      | "duration_value" -> 
        let json = Js.Dict.unsafeGet json "duration_value" in
        (MetricTypes.Duration_value ((decode_duration (Pbrt_bs.object_ json "get_response" "Duration_value"))) : MetricTypes.get_response)
      | "histo_duration" -> 
        let json = Js.Dict.unsafeGet json "histo_duration" in
        (MetricTypes.Histo_duration ((decode_histo_duration_stat (Pbrt_bs.object_ json "get_response" "Histo_duration"))) : MetricTypes.get_response)
      | "top_duration" -> 
        let json = Js.Dict.unsafeGet json "top_duration" in
        (MetricTypes.Top_duration ((decode_terms_count_duration (Pbrt_bs.object_ json "get_response" "Top_duration"))) : MetricTypes.get_response)
      
      | _ -> loop (i - 1)
      end
  in
  loop (Array.length keys - 1)

let rec decode_info_response json =
  let keys = Js.Dict.keys json in
  let rec loop = function 
    | -1 -> Pbrt_bs.E.malformed_variant "info_response"
    | i -> 
      begin match Array.unsafe_get keys i with
      | "error" -> 
        let json = Js.Dict.unsafeGet json "error" in
        (MetricTypes.Error (Pbrt_bs.string json "info_response" "Error") : MetricTypes.info_response)
      | "info" -> 
        let json = Js.Dict.unsafeGet json "info" in
        (MetricTypes.Info ((decode_metric_info (Pbrt_bs.object_ json "info_response" "Info"))) : MetricTypes.info_response)
      
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
  begin match v.MetricTypes.metrics with
  | [] -> ()
  | __x__ -> (* metrics *)
    let (metrics':Js.Json.t) =
      __x__
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_metric_info |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "metrics" metrics'
  end;
  json

let rec encode_trend (v:MetricTypes.trend) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "interval" (Js.Json.string v.MetricTypes.interval);
  json

let rec encode_top (v:MetricTypes.top) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "limit" (Js.Json.number (Int32.to_float v.MetricTypes.limit));
  json

let rec encode_compute (v:MetricTypes.compute) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "void" (Js.Json.string v.MetricTypes.void);
  json

let rec encode_get_request_options (v:MetricTypes.get_request_options) = 
  let json = Js.Dict.empty () in
  begin match v with
  | MetricTypes.Compute v ->
    begin (* compute field *)
      let json' = encode_compute v in
      Js.Dict.set json "compute" (Js.Json.object_ json');
    end;
  | MetricTypes.Trend v ->
    begin (* trend field *)
      let json' = encode_trend v in
      Js.Dict.set json "trend" (Js.Json.object_ json');
    end;
  | MetricTypes.Top v ->
    begin (* top field *)
      let json' = encode_top v in
      Js.Dict.set json "top" (Js.Json.object_ json');
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
    | Compute v ->
      begin (* compute field *)
        let json' = encode_compute v in
        Js.Dict.set json "compute" (Js.Json.object_ json');
      end;
    | Trend v ->
      begin (* trend field *)
        let json' = encode_trend v in
        Js.Dict.set json "trend" (Js.Json.object_ json');
      end;
    | Top v ->
      begin (* top field *)
        let json' = encode_top v in
        Js.Dict.set json "top" (Js.Json.object_ json');
      end;
  end; (* match v.options *)
  json

let rec encode_info_request (v:MetricTypes.info_request) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "metric" (Js.Json.string v.MetricTypes.metric);
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
  begin match v.MetricTypes.histo with
  | [] -> ()
  | __x__ -> (* histo *)
    let (histo':Js.Json.t) =
      __x__
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_histo_int |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "histo" histo'
  end;
  json

let rec encode_histo_float_stat (v:MetricTypes.histo_float_stat) = 
  let json = Js.Dict.empty () in
  begin match v.MetricTypes.histo with
  | [] -> ()
  | __x__ -> (* histo *)
    let (histo':Js.Json.t) =
      __x__
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_histo_float |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "histo" histo'
  end;
  json

let rec encode_term_count_int (v:MetricTypes.term_count_int) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "term" (Js.Json.string v.MetricTypes.term);
  Js.Dict.set json "count" (Js.Json.number (Int32.to_float v.MetricTypes.count));
  json

let rec encode_terms_count_int (v:MetricTypes.terms_count_int) = 
  let json = Js.Dict.empty () in
  begin match v.MetricTypes.termcount with
  | [] -> ()
  | __x__ -> (* termcount *)
    let (termcount':Js.Json.t) =
      __x__
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_term_count_int |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "termcount" termcount'
  end;
  Js.Dict.set json "total_hits" (Js.Json.number (Int32.to_float v.MetricTypes.total_hits));
  json

let rec encode_term_count_float (v:MetricTypes.term_count_float) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "term" (Js.Json.string v.MetricTypes.term);
  Js.Dict.set json "count" (Js.Json.number v.MetricTypes.count);
  json

let rec encode_terms_count_float (v:MetricTypes.terms_count_float) = 
  let json = Js.Dict.empty () in
  begin match v.MetricTypes.termcount with
  | [] -> ()
  | __x__ -> (* termcount *)
    let (termcount':Js.Json.t) =
      __x__
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_term_count_float |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "termcount" termcount'
  end;
  Js.Dict.set json "total_hits" (Js.Json.number (Int32.to_float v.MetricTypes.total_hits));
  json

let rec encode_duration (v:MetricTypes.duration) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "value" (Js.Json.number (Int32.to_float v.MetricTypes.value));
  json

let rec encode_histo_duration (v:MetricTypes.histo_duration) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "date" (Js.Json.string v.MetricTypes.date);
  Js.Dict.set json "count" (Js.Json.number (Int32.to_float v.MetricTypes.count));
  json

let rec encode_histo_duration_stat (v:MetricTypes.histo_duration_stat) = 
  let json = Js.Dict.empty () in
  begin match v.MetricTypes.histo with
  | [] -> ()
  | __x__ -> (* histo *)
    let (histo':Js.Json.t) =
      __x__
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_histo_duration |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "histo" histo'
  end;
  json

let rec encode_term_count_duration (v:MetricTypes.term_count_duration) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "term" (Js.Json.string v.MetricTypes.term);
  Js.Dict.set json "count" (Js.Json.number (Int32.to_float v.MetricTypes.count));
  json

let rec encode_terms_count_duration (v:MetricTypes.terms_count_duration) = 
  let json = Js.Dict.empty () in
  begin match v.MetricTypes.termcount with
  | [] -> ()
  | __x__ -> (* termcount *)
    let (termcount':Js.Json.t) =
      __x__
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_term_count_duration |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "termcount" termcount'
  end;
  Js.Dict.set json "total_hits" (Js.Json.number (Int32.to_float v.MetricTypes.total_hits));
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
  | MetricTypes.Histo_int v ->
    begin (* histoInt field *)
      let json' = encode_histo_int_stat v in
      Js.Dict.set json "histo_int" (Js.Json.object_ json');
    end;
  | MetricTypes.Histo_float v ->
    begin (* histoFloat field *)
      let json' = encode_histo_float_stat v in
      Js.Dict.set json "histo_float" (Js.Json.object_ json');
    end;
  | MetricTypes.Top_int v ->
    begin (* topInt field *)
      let json' = encode_terms_count_int v in
      Js.Dict.set json "top_int" (Js.Json.object_ json');
    end;
  | MetricTypes.Top_float v ->
    begin (* topFloat field *)
      let json' = encode_terms_count_float v in
      Js.Dict.set json "top_float" (Js.Json.object_ json');
    end;
  | MetricTypes.Duration_value v ->
    begin (* durationValue field *)
      let json' = encode_duration v in
      Js.Dict.set json "duration_value" (Js.Json.object_ json');
    end;
  | MetricTypes.Histo_duration v ->
    begin (* histoDuration field *)
      let json' = encode_histo_duration_stat v in
      Js.Dict.set json "histo_duration" (Js.Json.object_ json');
    end;
  | MetricTypes.Top_duration v ->
    begin (* topDuration field *)
      let json' = encode_terms_count_duration v in
      Js.Dict.set json "top_duration" (Js.Json.object_ json');
    end;
  end;
  json

let rec encode_info_response (v:MetricTypes.info_response) = 
  let json = Js.Dict.empty () in
  begin match v with
  | MetricTypes.Error v ->
    Js.Dict.set json "error" (Js.Json.string v);
  | MetricTypes.Info v ->
    begin (* info field *)
      let json' = encode_metric_info v in
      Js.Dict.set json "info" (Js.Json.object_ json');
    end;
  end;
  json
