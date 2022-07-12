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

type get_request = {
  index : string;
  username : string;
  query : string;
  metric : string;
}

type histo = {
  date : string;
  count : int32;
}

type histo_stat = {
  histo : histo list;
}

type get_response =
  | Error of string
  | Float_value of float
  | Int_value of int32
  | Histo_value of histo_stat


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

val default_get_request : 
  ?index:string ->
  ?username:string ->
  ?query:string ->
  ?metric:string ->
  unit ->
  get_request
(** [default_get_request ()] is the default value for type [get_request] *)

val default_histo : 
  ?date:string ->
  ?count:int32 ->
  unit ->
  histo
(** [default_histo ()] is the default value for type [histo] *)

val default_histo_stat : 
  ?histo:histo list ->
  unit ->
  histo_stat
(** [default_histo_stat ()] is the default value for type [histo_stat] *)

val default_get_response : unit -> get_response
(** [default_get_response ()] is the default value for type [get_response] *)
