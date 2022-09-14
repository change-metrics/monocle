[@@@ocaml.warning "-27-30-39"]


type metric_info = {
  name : string;
  description : string;
  long_description : string;
  metric : string;
}

type list_request = {
  void : string;
}

type list_response = {
  metrics : metric_info list;
}

type trend = {
  interval : string;
}

type top = {
  limit : int32;
}

type compute = {
  void : string;
}

type get_request_options =
  | Compute of compute
  | Trend of trend
  | Top of top

and get_request = {
  index : string;
  username : string;
  query : string;
  metric : string;
  options : get_request_options;
}

type info_request = {
  metric : string;
}

type histo_int = {
  date : string;
  count : int32;
}

type histo_float = {
  date : string;
  count : float;
}

type histo_int_stat = {
  histo : histo_int list;
}

type histo_float_stat = {
  histo : histo_float list;
}

type term_count_int = {
  term : string;
  count : int32;
}

type terms_count_int = {
  termcount : term_count_int list;
  total_hits : int32;
}

type term_count_float = {
  term : string;
  count : float;
}

type terms_count_float = {
  termcount : term_count_float list;
  total_hits : int32;
}

type duration = {
  value : int32;
}

type histo_duration = {
  date : string;
  count : int32;
}

type histo_duration_stat = {
  histo : histo_duration list;
}

type term_count_duration = {
  term : string;
  count : int32;
}

type terms_count_duration = {
  termcount : term_count_duration list;
  total_hits : int32;
}

type get_response =
  | Error of string
  | Float_value of float
  | Int_value of int32
  | Histo_int of histo_int_stat
  | Histo_float of histo_float_stat
  | Top_int of terms_count_int
  | Top_float of terms_count_float
  | Duration_value of duration
  | Histo_duration of histo_duration_stat
  | Top_duration of terms_count_duration

type info_response =
  | Error of string
  | Info of metric_info

let rec default_metric_info 
  ?name:((name:string) = "")
  ?description:((description:string) = "")
  ?long_description:((long_description:string) = "")
  ?metric:((metric:string) = "")
  () : metric_info  = {
  name;
  description;
  long_description;
  metric;
}

let rec default_list_request 
  ?void:((void:string) = "")
  () : list_request  = {
  void;
}

let rec default_list_response 
  ?metrics:((metrics:metric_info list) = [])
  () : list_response  = {
  metrics;
}

let rec default_trend 
  ?interval:((interval:string) = "")
  () : trend  = {
  interval;
}

let rec default_top 
  ?limit:((limit:int32) = 0l)
  () : top  = {
  limit;
}

let rec default_compute 
  ?void:((void:string) = "")
  () : compute  = {
  void;
}

let rec default_get_request_options () : get_request_options = Compute (default_compute ())

and default_get_request 
  ?index:((index:string) = "")
  ?username:((username:string) = "")
  ?query:((query:string) = "")
  ?metric:((metric:string) = "")
  ?options:((options:get_request_options) = Compute (default_compute ()))
  () : get_request  = {
  index;
  username;
  query;
  metric;
  options;
}

let rec default_info_request 
  ?metric:((metric:string) = "")
  () : info_request  = {
  metric;
}

let rec default_histo_int 
  ?date:((date:string) = "")
  ?count:((count:int32) = 0l)
  () : histo_int  = {
  date;
  count;
}

let rec default_histo_float 
  ?date:((date:string) = "")
  ?count:((count:float) = 0.)
  () : histo_float  = {
  date;
  count;
}

let rec default_histo_int_stat 
  ?histo:((histo:histo_int list) = [])
  () : histo_int_stat  = {
  histo;
}

let rec default_histo_float_stat 
  ?histo:((histo:histo_float list) = [])
  () : histo_float_stat  = {
  histo;
}

let rec default_term_count_int 
  ?term:((term:string) = "")
  ?count:((count:int32) = 0l)
  () : term_count_int  = {
  term;
  count;
}

let rec default_terms_count_int 
  ?termcount:((termcount:term_count_int list) = [])
  ?total_hits:((total_hits:int32) = 0l)
  () : terms_count_int  = {
  termcount;
  total_hits;
}

let rec default_term_count_float 
  ?term:((term:string) = "")
  ?count:((count:float) = 0.)
  () : term_count_float  = {
  term;
  count;
}

let rec default_terms_count_float 
  ?termcount:((termcount:term_count_float list) = [])
  ?total_hits:((total_hits:int32) = 0l)
  () : terms_count_float  = {
  termcount;
  total_hits;
}

let rec default_duration 
  ?value:((value:int32) = 0l)
  () : duration  = {
  value;
}

let rec default_histo_duration 
  ?date:((date:string) = "")
  ?count:((count:int32) = 0l)
  () : histo_duration  = {
  date;
  count;
}

let rec default_histo_duration_stat 
  ?histo:((histo:histo_duration list) = [])
  () : histo_duration_stat  = {
  histo;
}

let rec default_term_count_duration 
  ?term:((term:string) = "")
  ?count:((count:int32) = 0l)
  () : term_count_duration  = {
  term;
  count;
}

let rec default_terms_count_duration 
  ?termcount:((termcount:term_count_duration list) = [])
  ?total_hits:((total_hits:int32) = 0l)
  () : terms_count_duration  = {
  termcount;
  total_hits;
}

let rec default_get_response () : get_response = Error ("")

let rec default_info_response () : info_response = Error ("")
