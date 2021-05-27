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

type fields_request = {
  version : string;
}

type field_type =
  | Field_date 
  | Field_number 
  | Field_text 
  | Field_bool 
  | Field_regex 

type field = {
  name : string;
  description : string;
  type_ : field_type;
}

type fields_response = {
  fields : field list;
}

type query_error = {
  message : string;
  position : int32;
}

type changes_query_request = {
  index : string;
  query : string;
}

type change = {
  title : string;
  url : string;
  repository_fullname : string;
  state : string;
  branch : string;
  created_at : TimestampTypes.timestamp option;
  task_data : TaskDataTypes.new_task_data list;
}

type changes = {
  changes : change list;
}

type changes_query_response =
  | Error of query_error
  | Items of changes


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

val default_fields_request : 
  ?version:string ->
  unit ->
  fields_request
(** [default_fields_request ()] is the default value for type [fields_request] *)

val default_field_type : unit -> field_type
(** [default_field_type ()] is the default value for type [field_type] *)

val default_field : 
  ?name:string ->
  ?description:string ->
  ?type_:field_type ->
  unit ->
  field
(** [default_field ()] is the default value for type [field] *)

val default_fields_response : 
  ?fields:field list ->
  unit ->
  fields_response
(** [default_fields_response ()] is the default value for type [fields_response] *)

val default_query_error : 
  ?message:string ->
  ?position:int32 ->
  unit ->
  query_error
(** [default_query_error ()] is the default value for type [query_error] *)

val default_changes_query_request : 
  ?index:string ->
  ?query:string ->
  unit ->
  changes_query_request
(** [default_changes_query_request ()] is the default value for type [changes_query_request] *)

val default_change : 
  ?title:string ->
  ?url:string ->
  ?repository_fullname:string ->
  ?state:string ->
  ?branch:string ->
  ?created_at:TimestampTypes.timestamp option ->
  ?task_data:TaskDataTypes.new_task_data list ->
  unit ->
  change
(** [default_change ()] is the default value for type [change] *)

val default_changes : 
  ?changes:change list ->
  unit ->
  changes
(** [default_changes ()] is the default value for type [changes] *)

val default_changes_query_response : unit -> changes_query_response
(** [default_changes_query_response ()] is the default value for type [changes_query_response] *)
