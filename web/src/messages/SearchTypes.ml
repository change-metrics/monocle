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

type order_direction =
  | Asc 
  | Desc 

type order = {
  field : string;
  direction : order_direction;
}

type query_request_query_type =
  | Query_change 
  | Query_change_lifecycle 
  | Query_repos_summary 

type query_request = {
  index : string;
  username : string;
  query : string;
  query_type : query_request_query_type;
  order : order option;
  limit : int32;
}

type file = {
  additions : int32;
  deletions : int32;
  path : string;
}

type commit = {
  sha : string;
  title : string;
  author : string;
  authored_at : TimestampTypes.timestamp option;
  committer : string;
  committed_at : TimestampTypes.timestamp option;
  additions : int32;
  deletions : int32;
}

type change_merged_by_m =
  | Merged_by of string

and change = {
  change_id : string;
  author : string;
  title : string;
  url : string;
  repository_fullname : string;
  state : string;
  branch : string;
  target_branch : string;
  created_at : TimestampTypes.timestamp option;
  updated_at : TimestampTypes.timestamp option;
  merged_at : TimestampTypes.timestamp option;
  merged_by_m : change_merged_by_m;
  text : string;
  additions : int32;
  deletions : int32;
  approval : string list;
  assignees : string list;
  labels : string list;
  draft : bool;
  mergeable : bool;
  changed_files : file list;
  changed_files_count : int32;
  commits : commit list;
  commits_count : int32;
  task_data : TaskDataTypes.task_data list;
}

type changes = {
  changes : change list;
}

type repo_summary = {
  fullname : string;
  total_changes : int32;
  abandoned_changes : int32;
  merged_changes : int32;
  open_changes : int32;
}

type repos_summary = {
  repository_summary : repo_summary list;
}

type query_response =
  | Error of query_error
  | Changes of changes
  | Repos_summary of repos_summary

type changes_histos_event = {
  doc_count : int32;
  key : int64;
  key_as_string : string;
}

type changes_histos = {
  change_abandoned_event : changes_histos_event list;
  change_commit_force_pushed_event : changes_histos_event list;
  change_commit_pushed_event : changes_histos_event list;
  change_created_event : changes_histos_event list;
  change_merged_event : changes_histos_event list;
}

type changes_lifecycle_event = {
  authors_count : int32;
  events_count : int32;
}

type changes_lifecycle_ratios = {
  abandoned : float;
  iterations : float;
  merged : float;
  self_merged : float;
}

type changes_lifecycle = {
  change_commit_force_pushed_event : changes_lifecycle_event option;
  change_commit_pushed_event : changes_lifecycle_event option;
  change_created_event : changes_lifecycle_event option;
  abandoned : int32;
  commits : float;
  duration : float;
  duration_variability : float;
  histos : changes_histos option;
  merged : int32;
  opened : int32;
  ratios : changes_lifecycle_ratios option;
  self_merged : int32;
  tests : float;
}

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

let rec default_field_type () = (Field_date:field_type)

let rec default_field 
  ?name:((name:string) = "")
  ?description:((description:string) = "")
  ?type_:((type_:field_type) = default_field_type ())
  () : field  = {
  name;
  description;
  type_;
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

let rec default_order_direction () = (Asc:order_direction)

let rec default_order 
  ?field:((field:string) = "")
  ?direction:((direction:order_direction) = default_order_direction ())
  () : order  = {
  field;
  direction;
}

let rec default_query_request_query_type () = (Query_change:query_request_query_type)

let rec default_query_request 
  ?index:((index:string) = "")
  ?username:((username:string) = "")
  ?query:((query:string) = "")
  ?query_type:((query_type:query_request_query_type) = default_query_request_query_type ())
  ?order:((order:order option) = None)
  ?limit:((limit:int32) = 0l)
  () : query_request  = {
  index;
  username;
  query;
  query_type;
  order;
  limit;
}

let rec default_file 
  ?additions:((additions:int32) = 0l)
  ?deletions:((deletions:int32) = 0l)
  ?path:((path:string) = "")
  () : file  = {
  additions;
  deletions;
  path;
}

let rec default_commit 
  ?sha:((sha:string) = "")
  ?title:((title:string) = "")
  ?author:((author:string) = "")
  ?authored_at:((authored_at:TimestampTypes.timestamp option) = None)
  ?committer:((committer:string) = "")
  ?committed_at:((committed_at:TimestampTypes.timestamp option) = None)
  ?additions:((additions:int32) = 0l)
  ?deletions:((deletions:int32) = 0l)
  () : commit  = {
  sha;
  title;
  author;
  authored_at;
  committer;
  committed_at;
  additions;
  deletions;
}

let rec default_change_merged_by_m () : change_merged_by_m = Merged_by ("")

and default_change 
  ?change_id:((change_id:string) = "")
  ?author:((author:string) = "")
  ?title:((title:string) = "")
  ?url:((url:string) = "")
  ?repository_fullname:((repository_fullname:string) = "")
  ?state:((state:string) = "")
  ?branch:((branch:string) = "")
  ?target_branch:((target_branch:string) = "")
  ?created_at:((created_at:TimestampTypes.timestamp option) = None)
  ?updated_at:((updated_at:TimestampTypes.timestamp option) = None)
  ?merged_at:((merged_at:TimestampTypes.timestamp option) = None)
  ?merged_by_m:((merged_by_m:change_merged_by_m) = Merged_by (""))
  ?text:((text:string) = "")
  ?additions:((additions:int32) = 0l)
  ?deletions:((deletions:int32) = 0l)
  ?approval:((approval:string list) = [])
  ?assignees:((assignees:string list) = [])
  ?labels:((labels:string list) = [])
  ?draft:((draft:bool) = false)
  ?mergeable:((mergeable:bool) = false)
  ?changed_files:((changed_files:file list) = [])
  ?changed_files_count:((changed_files_count:int32) = 0l)
  ?commits:((commits:commit list) = [])
  ?commits_count:((commits_count:int32) = 0l)
  ?task_data:((task_data:TaskDataTypes.task_data list) = [])
  () : change  = {
  change_id;
  author;
  title;
  url;
  repository_fullname;
  state;
  branch;
  target_branch;
  created_at;
  updated_at;
  merged_at;
  merged_by_m;
  text;
  additions;
  deletions;
  approval;
  assignees;
  labels;
  draft;
  mergeable;
  changed_files;
  changed_files_count;
  commits;
  commits_count;
  task_data;
}

let rec default_changes 
  ?changes:((changes:change list) = [])
  () : changes  = {
  changes;
}

let rec default_repo_summary 
  ?fullname:((fullname:string) = "")
  ?total_changes:((total_changes:int32) = 0l)
  ?abandoned_changes:((abandoned_changes:int32) = 0l)
  ?merged_changes:((merged_changes:int32) = 0l)
  ?open_changes:((open_changes:int32) = 0l)
  () : repo_summary  = {
  fullname;
  total_changes;
  abandoned_changes;
  merged_changes;
  open_changes;
}

let rec default_repos_summary 
  ?repository_summary:((repository_summary:repo_summary list) = [])
  () : repos_summary  = {
  repository_summary;
}

let rec default_query_response () : query_response = Error (default_query_error ())

let rec default_changes_histos_event 
  ?doc_count:((doc_count:int32) = 0l)
  ?key:((key:int64) = 0L)
  ?key_as_string:((key_as_string:string) = "")
  () : changes_histos_event  = {
  doc_count;
  key;
  key_as_string;
}

let rec default_changes_histos 
  ?change_abandoned_event:((change_abandoned_event:changes_histos_event list) = [])
  ?change_commit_force_pushed_event:((change_commit_force_pushed_event:changes_histos_event list) = [])
  ?change_commit_pushed_event:((change_commit_pushed_event:changes_histos_event list) = [])
  ?change_created_event:((change_created_event:changes_histos_event list) = [])
  ?change_merged_event:((change_merged_event:changes_histos_event list) = [])
  () : changes_histos  = {
  change_abandoned_event;
  change_commit_force_pushed_event;
  change_commit_pushed_event;
  change_created_event;
  change_merged_event;
}

let rec default_changes_lifecycle_event 
  ?authors_count:((authors_count:int32) = 0l)
  ?events_count:((events_count:int32) = 0l)
  () : changes_lifecycle_event  = {
  authors_count;
  events_count;
}

let rec default_changes_lifecycle_ratios 
  ?abandoned:((abandoned:float) = 0.)
  ?iterations:((iterations:float) = 0.)
  ?merged:((merged:float) = 0.)
  ?self_merged:((self_merged:float) = 0.)
  () : changes_lifecycle_ratios  = {
  abandoned;
  iterations;
  merged;
  self_merged;
}

let rec default_changes_lifecycle 
  ?change_commit_force_pushed_event:((change_commit_force_pushed_event:changes_lifecycle_event option) = None)
  ?change_commit_pushed_event:((change_commit_pushed_event:changes_lifecycle_event option) = None)
  ?change_created_event:((change_created_event:changes_lifecycle_event option) = None)
  ?abandoned:((abandoned:int32) = 0l)
  ?commits:((commits:float) = 0.)
  ?duration:((duration:float) = 0.)
  ?duration_variability:((duration_variability:float) = 0.)
  ?histos:((histos:changes_histos option) = None)
  ?merged:((merged:int32) = 0l)
  ?opened:((opened:int32) = 0l)
  ?ratios:((ratios:changes_lifecycle_ratios option) = None)
  ?self_merged:((self_merged:int32) = 0l)
  ?tests:((tests:float) = 0.)
  () : changes_lifecycle  = {
  change_commit_force_pushed_event;
  change_commit_pushed_event;
  change_created_event;
  abandoned;
  commits;
  duration;
  duration_variability;
  histos;
  merged;
  opened;
  ratios;
  self_merged;
  tests;
}
