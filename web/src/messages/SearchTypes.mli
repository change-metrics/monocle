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
  | Query_top_authors_changes_created 
  | Query_top_authors_changes_merged 
  | Query_top_authors_changes_reviewed 
  | Query_top_authors_changes_commented 
  | Query_top_reviewed_authors 
  | Query_top_commented_authors 

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
  reposum : repo_summary list;
}

type term_count = {
  term : string;
  count : int32;
}

type terms_count = {
  termcount : term_count list;
}

type query_response =
  | Error of query_error
  | Changes of changes
  | Repos_summary of repos_summary
  | Top_authors of terms_count

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

val default_order_direction : unit -> order_direction
(** [default_order_direction ()] is the default value for type [order_direction] *)

val default_order : 
  ?field:string ->
  ?direction:order_direction ->
  unit ->
  order
(** [default_order ()] is the default value for type [order] *)

val default_query_request_query_type : unit -> query_request_query_type
(** [default_query_request_query_type ()] is the default value for type [query_request_query_type] *)

val default_query_request : 
  ?index:string ->
  ?username:string ->
  ?query:string ->
  ?query_type:query_request_query_type ->
  ?order:order option ->
  ?limit:int32 ->
  unit ->
  query_request
(** [default_query_request ()] is the default value for type [query_request] *)

val default_file : 
  ?additions:int32 ->
  ?deletions:int32 ->
  ?path:string ->
  unit ->
  file
(** [default_file ()] is the default value for type [file] *)

val default_commit : 
  ?sha:string ->
  ?title:string ->
  ?author:string ->
  ?authored_at:TimestampTypes.timestamp option ->
  ?committer:string ->
  ?committed_at:TimestampTypes.timestamp option ->
  ?additions:int32 ->
  ?deletions:int32 ->
  unit ->
  commit
(** [default_commit ()] is the default value for type [commit] *)

val default_change_merged_by_m : unit -> change_merged_by_m
(** [default_change_merged_by_m ()] is the default value for type [change_merged_by_m] *)

val default_change : 
  ?change_id:string ->
  ?author:string ->
  ?title:string ->
  ?url:string ->
  ?repository_fullname:string ->
  ?state:string ->
  ?branch:string ->
  ?target_branch:string ->
  ?created_at:TimestampTypes.timestamp option ->
  ?updated_at:TimestampTypes.timestamp option ->
  ?merged_at:TimestampTypes.timestamp option ->
  ?merged_by_m:change_merged_by_m ->
  ?text:string ->
  ?additions:int32 ->
  ?deletions:int32 ->
  ?approval:string list ->
  ?assignees:string list ->
  ?labels:string list ->
  ?draft:bool ->
  ?mergeable:bool ->
  ?changed_files:file list ->
  ?changed_files_count:int32 ->
  ?commits:commit list ->
  ?commits_count:int32 ->
  ?task_data:TaskDataTypes.task_data list ->
  unit ->
  change
(** [default_change ()] is the default value for type [change] *)

val default_changes : 
  ?changes:change list ->
  unit ->
  changes
(** [default_changes ()] is the default value for type [changes] *)

val default_repo_summary : 
  ?fullname:string ->
  ?total_changes:int32 ->
  ?abandoned_changes:int32 ->
  ?merged_changes:int32 ->
  ?open_changes:int32 ->
  unit ->
  repo_summary
(** [default_repo_summary ()] is the default value for type [repo_summary] *)

val default_repos_summary : 
  ?reposum:repo_summary list ->
  unit ->
  repos_summary
(** [default_repos_summary ()] is the default value for type [repos_summary] *)

val default_term_count : 
  ?term:string ->
  ?count:int32 ->
  unit ->
  term_count
(** [default_term_count ()] is the default value for type [term_count] *)

val default_terms_count : 
  ?termcount:term_count list ->
  unit ->
  terms_count
(** [default_terms_count ()] is the default value for type [terms_count] *)

val default_query_response : unit -> query_response
(** [default_query_response ()] is the default value for type [query_response] *)

val default_changes_histos_event : 
  ?doc_count:int32 ->
  ?key:int64 ->
  ?key_as_string:string ->
  unit ->
  changes_histos_event
(** [default_changes_histos_event ()] is the default value for type [changes_histos_event] *)

val default_changes_histos : 
  ?change_abandoned_event:changes_histos_event list ->
  ?change_commit_force_pushed_event:changes_histos_event list ->
  ?change_commit_pushed_event:changes_histos_event list ->
  ?change_created_event:changes_histos_event list ->
  ?change_merged_event:changes_histos_event list ->
  unit ->
  changes_histos
(** [default_changes_histos ()] is the default value for type [changes_histos] *)

val default_changes_lifecycle_event : 
  ?authors_count:int32 ->
  ?events_count:int32 ->
  unit ->
  changes_lifecycle_event
(** [default_changes_lifecycle_event ()] is the default value for type [changes_lifecycle_event] *)

val default_changes_lifecycle_ratios : 
  ?abandoned:float ->
  ?iterations:float ->
  ?merged:float ->
  ?self_merged:float ->
  unit ->
  changes_lifecycle_ratios
(** [default_changes_lifecycle_ratios ()] is the default value for type [changes_lifecycle_ratios] *)

val default_changes_lifecycle : 
  ?change_commit_force_pushed_event:changes_lifecycle_event option ->
  ?change_commit_pushed_event:changes_lifecycle_event option ->
  ?change_created_event:changes_lifecycle_event option ->
  ?abandoned:int32 ->
  ?commits:float ->
  ?duration:float ->
  ?duration_variability:float ->
  ?histos:changes_histos option ->
  ?merged:int32 ->
  ?opened:int32 ->
  ?ratios:changes_lifecycle_ratios option ->
  ?self_merged:int32 ->
  ?tests:float ->
  unit ->
  changes_lifecycle
(** [default_changes_lifecycle ()] is the default value for type [changes_lifecycle] *)
