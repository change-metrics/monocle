[@@@ocaml.warning "-27-30-39"]


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

let rec default_task_data_commit_request 
  ?index:((index:string) = "")
  ?crawler:((crawler:string) = "")
  ?apikey:((apikey:string) = "")
  ?timestamp:((timestamp:TimestampTypes.timestamp option) = None)
  () : task_data_commit_request  = {
  index;
  crawler;
  apikey;
  timestamp;
}

let rec default_task_data_commit_error () = (Unknown_index:task_data_commit_error)

let rec default_task_data_commit_response () : task_data_commit_response = Error (default_task_data_commit_error ())

let rec default_task_data_get_last_updated_error () = (Get_unknown_index:task_data_get_last_updated_error)

let rec default_task_data_get_last_updated_request 
  ?index:((index:string) = "")
  ?crawler:((crawler:string) = "")
  () : task_data_get_last_updated_request  = {
  index;
  crawler;
}

let rec default_task_data_get_last_updated_response () : task_data_get_last_updated_response = Error (default_task_data_get_last_updated_error ())
