(** search.proto Types *)



(** {2 Types} *)

type search_suggestions_request = {
  index : string;
}

type search_suggestions_response = {
  task_types : string list;
  authors : string list;
  approvals : string list;
  priorities : string list;
  severities : string list;
}


(** {2 Default values} *)

val default_search_suggestions_request : 
  ?index:string ->
  unit ->
  search_suggestions_request
(** [default_search_suggestions_request ()] is the default value for type [search_suggestions_request] *)

val default_search_suggestions_response : 
  ?task_types:string list ->
  ?authors:string list ->
  ?approvals:string list ->
  ?priorities:string list ->
  ?severities:string list ->
  unit ->
  search_suggestions_response
(** [default_search_suggestions_response ()] is the default value for type [search_suggestions_response] *)
