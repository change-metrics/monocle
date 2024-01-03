(** crawler.proto Types *)



(** {2 Types} *)

type entity =
  | Organization_name of string
  | Project_name of string
  | Project_issue_name of string
  | Td_name of string
  | User_name of string

type entity_type =
  | Entity_type_organization 
  | Entity_type_project 
  | Entity_type_task_data 
  | Entity_type_user 

type crawler_error = {
  message : string;
  body : string;
  created_at : TimestampTypes.timestamp option;
}

type crawler_error_list = {
  crawler : string;
  entity : entity option;
  errors : crawler_error list;
}

type errors_request = {
  index : string;
  query : string;
}

type errors_list = {
  errors : crawler_error_list list;
}

type errors_response =
  | Success of errors_list
  | Error of string

type project = {
  full_path : string;
}

type add_doc_request = {
  index : string;
  crawler : string;
  apikey : string;
  entity : entity option;
  changes : ChangeTypes.change list;
  events : ChangeTypes.change_event list;
  projects : project list;
  task_datas : SearchTypes.task_data list;
  issues : IssueTypes.issue list;
  issue_events : IssueTypes.issue_event list;
  errors : crawler_error list;
}

type add_doc_error =
  | Add_unknown_index 
  | Add_unknown_crawler 
  | Add_unknown_api_key 
  | Add_failed 

type add_doc_response =
  | Error of add_doc_error

type commit_request = {
  index : string;
  crawler : string;
  apikey : string;
  entity : entity option;
  timestamp : TimestampTypes.timestamp option;
}

type commit_error =
  | Commit_unknown_index 
  | Commit_unknown_crawler 
  | Commit_unknown_api_key 
  | Commit_date_inferior_than_previous 
  | Commit_date_missing 

type commit_response =
  | Error of commit_error
  | Timestamp of TimestampTypes.timestamp

type commit_info_request = {
  index : string;
  crawler : string;
  entity : entity_type;
  offset : int32;
}

type commit_info_error =
  | Commit_get_unknown_index 
  | Commit_get_unknown_crawler 
  | Commit_get_no_entity 

type commit_info_response_oldest_entity = {
  entity : entity option;
  last_commit_at : TimestampTypes.timestamp option;
}

type commit_info_response =
  | Error of commit_info_error
  | Entity of commit_info_response_oldest_entity


(** {2 Default values} *)

val default_entity : unit -> entity
(** [default_entity ()] is the default value for type [entity] *)

val default_entity_type : unit -> entity_type
(** [default_entity_type ()] is the default value for type [entity_type] *)

val default_crawler_error : 
  ?message:string ->
  ?body:string ->
  ?created_at:TimestampTypes.timestamp option ->
  unit ->
  crawler_error
(** [default_crawler_error ()] is the default value for type [crawler_error] *)

val default_crawler_error_list : 
  ?crawler:string ->
  ?entity:entity option ->
  ?errors:crawler_error list ->
  unit ->
  crawler_error_list
(** [default_crawler_error_list ()] is the default value for type [crawler_error_list] *)

val default_errors_request : 
  ?index:string ->
  ?query:string ->
  unit ->
  errors_request
(** [default_errors_request ()] is the default value for type [errors_request] *)

val default_errors_list : 
  ?errors:crawler_error_list list ->
  unit ->
  errors_list
(** [default_errors_list ()] is the default value for type [errors_list] *)

val default_errors_response : unit -> errors_response
(** [default_errors_response ()] is the default value for type [errors_response] *)

val default_project : 
  ?full_path:string ->
  unit ->
  project
(** [default_project ()] is the default value for type [project] *)

val default_add_doc_request : 
  ?index:string ->
  ?crawler:string ->
  ?apikey:string ->
  ?entity:entity option ->
  ?changes:ChangeTypes.change list ->
  ?events:ChangeTypes.change_event list ->
  ?projects:project list ->
  ?task_datas:SearchTypes.task_data list ->
  ?issues:IssueTypes.issue list ->
  ?issue_events:IssueTypes.issue_event list ->
  ?errors:crawler_error list ->
  unit ->
  add_doc_request
(** [default_add_doc_request ()] is the default value for type [add_doc_request] *)

val default_add_doc_error : unit -> add_doc_error
(** [default_add_doc_error ()] is the default value for type [add_doc_error] *)

val default_add_doc_response : unit -> add_doc_response
(** [default_add_doc_response ()] is the default value for type [add_doc_response] *)

val default_commit_request : 
  ?index:string ->
  ?crawler:string ->
  ?apikey:string ->
  ?entity:entity option ->
  ?timestamp:TimestampTypes.timestamp option ->
  unit ->
  commit_request
(** [default_commit_request ()] is the default value for type [commit_request] *)

val default_commit_error : unit -> commit_error
(** [default_commit_error ()] is the default value for type [commit_error] *)

val default_commit_response : unit -> commit_response
(** [default_commit_response ()] is the default value for type [commit_response] *)

val default_commit_info_request : 
  ?index:string ->
  ?crawler:string ->
  ?entity:entity_type ->
  ?offset:int32 ->
  unit ->
  commit_info_request
(** [default_commit_info_request ()] is the default value for type [commit_info_request] *)

val default_commit_info_error : unit -> commit_info_error
(** [default_commit_info_error ()] is the default value for type [commit_info_error] *)

val default_commit_info_response_oldest_entity : 
  ?entity:entity option ->
  ?last_commit_at:TimestampTypes.timestamp option ->
  unit ->
  commit_info_response_oldest_entity
(** [default_commit_info_response_oldest_entity ()] is the default value for type [commit_info_response_oldest_entity] *)

val default_commit_info_response : unit -> commit_info_response
(** [default_commit_info_response ()] is the default value for type [commit_info_response] *)
