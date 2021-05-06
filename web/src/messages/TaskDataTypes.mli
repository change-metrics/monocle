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
