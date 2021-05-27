[@@@ocaml.warning "-27-30-39"]


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

type field = {
  name : string;
  description : string;
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
  created_at : TimestampTypes.timestamp option;
}

type changes = {
  changes : change list;
}

type changes_query_response =
  | Error of query_error
  | Items of changes

let rec default_search_suggestions_request 
  ?index:((index:string) = "")
  () : search_suggestions_request  = {
  index;
}

let rec default_search_suggestions_response 
  ?task_types:((task_types:string list) = [])
  ?authors:((authors:string list) = [])
  ?approvals:((approvals:string list) = [])
  ?priorities:((priorities:string list) = [])
  ?severities:((severities:string list) = [])
  () : search_suggestions_response  = {
  task_types;
  authors;
  approvals;
  priorities;
  severities;
}

let rec default_fields_request 
  ?version:((version:string) = "")
  () : fields_request  = {
  version;
}

let rec default_field 
  ?name:((name:string) = "")
  ?description:((description:string) = "")
  () : field  = {
  name;
  description;
}

let rec default_fields_response 
  ?fields:((fields:field list) = [])
  () : fields_response  = {
  fields;
}

let rec default_query_error 
  ?message:((message:string) = "")
  ?position:((position:int32) = 0l)
  () : query_error  = {
  message;
  position;
}

let rec default_changes_query_request 
  ?index:((index:string) = "")
  ?query:((query:string) = "")
  () : changes_query_request  = {
  index;
  query;
}

let rec default_change 
  ?title:((title:string) = "")
  ?url:((url:string) = "")
  ?created_at:((created_at:TimestampTypes.timestamp option) = None)
  () : change  = {
  title;
  url;
  created_at;
}

let rec default_changes 
  ?changes:((changes:change list) = [])
  () : changes  = {
  changes;
}

let rec default_changes_query_response () : changes_query_response = Error (default_query_error ())
