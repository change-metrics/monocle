(** metric.proto Types *)



(** {2 Types} *)

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

type get_response =
  | Error of string
  | Float_value of float
  | Int_value of int32
  | Histo_int of histo_int_stat
  | Histo_float of histo_float_stat
  | Top_int of terms_count_int
  | Top_float of terms_count_float


(** {2 Default values} *)

val default_metric_info : 
  ?name:string ->
  ?description:string ->
  ?long_description:string ->
  ?metric:string ->
  unit ->
  metric_info
(** [default_metric_info ()] is the default value for type [metric_info] *)

val default_list_request : 
  ?void:string ->
  unit ->
  list_request
(** [default_list_request ()] is the default value for type [list_request] *)

val default_list_response : 
  ?metrics:metric_info list ->
  unit ->
  list_response
(** [default_list_response ()] is the default value for type [list_response] *)

val default_trend : 
  ?interval:string ->
  unit ->
  trend
(** [default_trend ()] is the default value for type [trend] *)

val default_top : 
  ?limit:int32 ->
  unit ->
  top
(** [default_top ()] is the default value for type [top] *)

val default_compute : 
  ?void:string ->
  unit ->
  compute
(** [default_compute ()] is the default value for type [compute] *)

val default_get_request_options : unit -> get_request_options
(** [default_get_request_options ()] is the default value for type [get_request_options] *)

val default_get_request : 
  ?index:string ->
  ?username:string ->
  ?query:string ->
  ?metric:string ->
  ?options:get_request_options ->
  unit ->
  get_request
(** [default_get_request ()] is the default value for type [get_request] *)

val default_histo_int : 
  ?date:string ->
  ?count:int32 ->
  unit ->
  histo_int
(** [default_histo_int ()] is the default value for type [histo_int] *)

val default_histo_float : 
  ?date:string ->
  ?count:float ->
  unit ->
  histo_float
(** [default_histo_float ()] is the default value for type [histo_float] *)

val default_histo_int_stat : 
  ?histo:histo_int list ->
  unit ->
  histo_int_stat
(** [default_histo_int_stat ()] is the default value for type [histo_int_stat] *)

val default_histo_float_stat : 
  ?histo:histo_float list ->
  unit ->
  histo_float_stat
(** [default_histo_float_stat ()] is the default value for type [histo_float_stat] *)

val default_term_count_int : 
  ?term:string ->
  ?count:int32 ->
  unit ->
  term_count_int
(** [default_term_count_int ()] is the default value for type [term_count_int] *)

val default_terms_count_int : 
  ?termcount:term_count_int list ->
  ?total_hits:int32 ->
  unit ->
  terms_count_int
(** [default_terms_count_int ()] is the default value for type [terms_count_int] *)

val default_term_count_float : 
  ?term:string ->
  ?count:float ->
  unit ->
  term_count_float
(** [default_term_count_float ()] is the default value for type [term_count_float] *)

val default_terms_count_float : 
  ?termcount:term_count_float list ->
  ?total_hits:int32 ->
  unit ->
  terms_count_float
(** [default_terms_count_float ()] is the default value for type [terms_count_float] *)

val default_get_response : unit -> get_response
(** [default_get_response ()] is the default value for type [get_response] *)
