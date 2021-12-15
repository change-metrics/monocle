(** metric.proto Types *)



(** {2 Types} *)

type metric_info = {
  name : string;
  description : string;
  long_description : string;
}

type list_request = {
  void : string;
}

type list_response = {
  metrics : metric_info list;
}


(** {2 Default values} *)

val default_metric_info : 
  ?name:string ->
  ?description:string ->
  ?long_description:string ->
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
