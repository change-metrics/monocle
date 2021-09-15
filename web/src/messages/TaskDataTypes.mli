(** task_data.proto Types *)



(** {2 Types} *)

type task_data_commit_request = {
  index : string;
  crawler : string;
  apikey : string;
  timestamp : TimestampTypes.timestamp option;
}

type task_data_commit_error =
  | Unknown_index 
  | Unknown_crawler 
  | Unknown_api_key 
  | Commit_date_inferior_than_previous 
  | Add_failed 

type task_data_commit_response =
  | Error of task_data_commit_error
  | Timestamp of TimestampTypes.timestamp

type task_data_get_last_updated_error =
  | Get_unknown_index 
  | Get_unknown_crawler 

type task_data_get_last_updated_request = {
  index : string;
  crawler : string;
}

type task_data_get_last_updated_response =
  | Error of task_data_get_last_updated_error
  | Timestamp of TimestampTypes.timestamp

type task_data = {
  updated_at : TimestampTypes.timestamp option;
  change_url : string;
  ttype : string list;
  tid : string;
  url : string;
  title : string;
  severity : string;
  priority : string;
  score : int32;
}

type task_data_add_request = {
  index : string;
  crawler : string;
  apikey : string;
  items : task_data list;
}

type task_data_add_response =
  | Error of task_data_commit_error


(** {2 Default values} *)

val default_task_data_commit_request : 
  ?index:string ->
  ?crawler:string ->
  ?apikey:string ->
  ?timestamp:TimestampTypes.timestamp option ->
  unit ->
  task_data_commit_request
(** [default_task_data_commit_request ()] is the default value for type [task_data_commit_request] *)

val default_task_data_commit_error : unit -> task_data_commit_error
(** [default_task_data_commit_error ()] is the default value for type [task_data_commit_error] *)

val default_task_data_commit_response : unit -> task_data_commit_response
(** [default_task_data_commit_response ()] is the default value for type [task_data_commit_response] *)

val default_task_data_get_last_updated_error : unit -> task_data_get_last_updated_error
(** [default_task_data_get_last_updated_error ()] is the default value for type [task_data_get_last_updated_error] *)

val default_task_data_get_last_updated_request : 
  ?index:string ->
  ?crawler:string ->
  unit ->
  task_data_get_last_updated_request
(** [default_task_data_get_last_updated_request ()] is the default value for type [task_data_get_last_updated_request] *)

val default_task_data_get_last_updated_response : unit -> task_data_get_last_updated_response
(** [default_task_data_get_last_updated_response ()] is the default value for type [task_data_get_last_updated_response] *)

val default_task_data : 
  ?updated_at:TimestampTypes.timestamp option ->
  ?change_url:string ->
  ?ttype:string list ->
  ?tid:string ->
  ?url:string ->
  ?title:string ->
  ?severity:string ->
  ?priority:string ->
  ?score:int32 ->
  unit ->
  task_data
(** [default_task_data ()] is the default value for type [task_data] *)

val default_task_data_add_request : 
  ?index:string ->
  ?crawler:string ->
  ?apikey:string ->
  ?items:task_data list ->
  unit ->
  task_data_add_request
(** [default_task_data_add_request ()] is the default value for type [task_data_add_request] *)

val default_task_data_add_response : unit -> task_data_add_response
(** [default_task_data_add_response ()] is the default value for type [task_data_add_response] *)
