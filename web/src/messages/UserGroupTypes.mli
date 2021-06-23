(** user_group.proto Types *)



(** {2 Types} *)

type group_definition = {
  name : string;
  members : int32;
}

type list_request = {
  index : string;
}

type list_response = {
  items : group_definition list;
}


(** {2 Default values} *)

val default_group_definition : 
  ?name:string ->
  ?members:int32 ->
  unit ->
  group_definition
(** [default_group_definition ()] is the default value for type [group_definition] *)

val default_list_request : 
  ?index:string ->
  unit ->
  list_request
(** [default_list_request ()] is the default value for type [list_request] *)

val default_list_response : 
  ?items:group_definition list ->
  unit ->
  list_response
(** [default_list_response ()] is the default value for type [list_response] *)
