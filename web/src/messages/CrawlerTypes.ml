[@@@ocaml.warning "-27-30-39"]


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
  entity : entity option;
}

type errors_request = {
  index : string;
  query : string;
}

type errors_list = {
  errors : crawler_error list;
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

let rec default_entity () : entity = Organization_name ("")

let rec default_entity_type () = (Entity_type_organization:entity_type)

let rec default_crawler_error 
  ?message:((message:string) = "")
  ?body:((body:string) = "")
  ?created_at:((created_at:TimestampTypes.timestamp option) = None)
  ?entity:((entity:entity option) = None)
  () : crawler_error  = {
  message;
  body;
  created_at;
  entity;
}

let rec default_errors_request 
  ?index:((index:string) = "")
  ?query:((query:string) = "")
  () : errors_request  = {
  index;
  query;
}

let rec default_errors_list 
  ?errors:((errors:crawler_error list) = [])
  () : errors_list  = {
  errors;
}

let rec default_errors_response () : errors_response = Success (default_errors_list ())

let rec default_project 
  ?full_path:((full_path:string) = "")
  () : project  = {
  full_path;
}

let rec default_add_doc_request 
  ?index:((index:string) = "")
  ?crawler:((crawler:string) = "")
  ?apikey:((apikey:string) = "")
  ?entity:((entity:entity option) = None)
  ?changes:((changes:ChangeTypes.change list) = [])
  ?events:((events:ChangeTypes.change_event list) = [])
  ?projects:((projects:project list) = [])
  ?task_datas:((task_datas:SearchTypes.task_data list) = [])
  ?issues:((issues:IssueTypes.issue list) = [])
  ?issue_events:((issue_events:IssueTypes.issue_event list) = [])
  ?errors:((errors:crawler_error list) = [])
  () : add_doc_request  = {
  index;
  crawler;
  apikey;
  entity;
  changes;
  events;
  projects;
  task_datas;
  issues;
  issue_events;
  errors;
}

let rec default_add_doc_error () = (Add_unknown_index:add_doc_error)

let rec default_add_doc_response () : add_doc_response = Error (default_add_doc_error ())

let rec default_commit_request 
  ?index:((index:string) = "")
  ?crawler:((crawler:string) = "")
  ?apikey:((apikey:string) = "")
  ?entity:((entity:entity option) = None)
  ?timestamp:((timestamp:TimestampTypes.timestamp option) = None)
  () : commit_request  = {
  index;
  crawler;
  apikey;
  entity;
  timestamp;
}

let rec default_commit_error () = (Commit_unknown_index:commit_error)

let rec default_commit_response () : commit_response = Error (default_commit_error ())

let rec default_commit_info_request 
  ?index:((index:string) = "")
  ?crawler:((crawler:string) = "")
  ?entity:((entity:entity_type) = default_entity_type ())
  ?offset:((offset:int32) = 0l)
  () : commit_info_request  = {
  index;
  crawler;
  entity;
  offset;
}

let rec default_commit_info_error () = (Commit_get_unknown_index:commit_info_error)

let rec default_commit_info_response_oldest_entity 
  ?entity:((entity:entity option) = None)
  ?last_commit_at:((last_commit_at:TimestampTypes.timestamp option) = None)
  () : commit_info_response_oldest_entity  = {
  entity;
  last_commit_at;
}

let rec default_commit_info_response () : commit_info_response = Error (default_commit_info_error ())
