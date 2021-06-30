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

type review_histo = {
  date : int64;
  count : int32;
}

type group_stat = {
  change_review_ratio : float;
  author_review_ratio : float;
  review_histo : review_histo list;
}

type user_stat = {
  name : string;
  stat : group_stat option;
}

type get_request = {
  index : string;
  name : string;
  query : string;
}

type get_response = {
  all : group_stat option;
  users : user_stat list;
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

val default_review_histo : 
  ?date:int64 ->
  ?count:int32 ->
  unit ->
  review_histo
(** [default_review_histo ()] is the default value for type [review_histo] *)

val default_group_stat : 
  ?change_review_ratio:float ->
  ?author_review_ratio:float ->
  ?review_histo:review_histo list ->
  unit ->
  group_stat
(** [default_group_stat ()] is the default value for type [group_stat] *)

val default_user_stat : 
  ?name:string ->
  ?stat:group_stat option ->
  unit ->
  user_stat
(** [default_user_stat ()] is the default value for type [user_stat] *)

val default_get_request : 
  ?index:string ->
  ?name:string ->
  ?query:string ->
  unit ->
  get_request
(** [default_get_request ()] is the default value for type [get_request] *)

val default_get_response : 
  ?all:group_stat option ->
  ?users:user_stat list ->
  unit ->
  get_response
(** [default_get_response ()] is the default value for type [get_response] *)
