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

type get_response =
  | Error of string
  | Float_value of float


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

val default_get_response : unit -> get_response
(** [default_get_response ()] is the default value for type [get_response] *)
