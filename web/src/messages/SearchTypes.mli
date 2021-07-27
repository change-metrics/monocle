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
  | Query_repos_summary 
  | Query_top_authors_changes_created 
  | Query_top_authors_changes_merged 
  | Query_top_authors_changes_reviewed 
  | Query_top_authors_changes_commented 
  | Query_top_reviewed_authors 
  | Query_top_commented_authors 
  | Query_top_authors_peers 
  | Query_new_changes_authors 
  | Query_changes_review_stats 
  | Query_changes_lifecycle_stats 
  | Query_active_authors_stats 

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

type review_count = {
  authors_count : int32;
  events_count : int32;
}

type histo = {
  date : string;
  count : int32;
}

type review_stats = {
  comment_count : review_count option;
  review_count : review_count option;
  comment_delay : int32;
  review_delay : int32;
  comment_histo : histo list;
  review_histo : histo list;
}

type activity_stats = {
  change_authors : int32;
  comment_authors : int32;
  review_authors : int32;
  comments_histo : histo list;
  reviews_histo : histo list;
  changes_histo : histo list;
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

type author_peer = {
  author : string;
  peer : string;
  strength : int32;
}

type authors_peers = {
  author_peer : author_peer list;
}

type lifecycle_stats = {
  created_histo : histo list;
  updated_histo : histo list;
  merged_histo : histo list;
  abandoned_histo : histo list;
  created : review_count option;
  opened : int32;
  abandoned : int32;
  abandoned_ratio : float;
  merged : int32;
  merged_ratio : float;
  self_merged : int32;
  self_merged_ratio : float;
  ttm_mean : float;
  ttm_variability : float;
  updates_of_changes : int32;
  changes_with_tests : float;
  iterations_per_change : float;
  commits_per_change : float;
}

type query_response =
  | Error of query_error
  | Changes of changes
  | Repos_summary of repos_summary
  | Top_authors of terms_count
  | Authors_peers of authors_peers
  | New_authors of terms_count
  | Review_stats of review_stats
  | Lifecycle_stats of lifecycle_stats
  | Activity_stats of activity_stats


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

val default_review_count : 
  ?authors_count:int32 ->
  ?events_count:int32 ->
  unit ->
  review_count
(** [default_review_count ()] is the default value for type [review_count] *)

val default_histo : 
  ?date:string ->
  ?count:int32 ->
  unit ->
  histo
(** [default_histo ()] is the default value for type [histo] *)

val default_review_stats : 
  ?comment_count:review_count option ->
  ?review_count:review_count option ->
  ?comment_delay:int32 ->
  ?review_delay:int32 ->
  ?comment_histo:histo list ->
  ?review_histo:histo list ->
  unit ->
  review_stats
(** [default_review_stats ()] is the default value for type [review_stats] *)

val default_activity_stats : 
  ?change_authors:int32 ->
  ?comment_authors:int32 ->
  ?review_authors:int32 ->
  ?comments_histo:histo list ->
  ?reviews_histo:histo list ->
  ?changes_histo:histo list ->
  unit ->
  activity_stats
(** [default_activity_stats ()] is the default value for type [activity_stats] *)

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

val default_author_peer : 
  ?author:string ->
  ?peer:string ->
  ?strength:int32 ->
  unit ->
  author_peer
(** [default_author_peer ()] is the default value for type [author_peer] *)

val default_authors_peers : 
  ?author_peer:author_peer list ->
  unit ->
  authors_peers
(** [default_authors_peers ()] is the default value for type [authors_peers] *)

val default_lifecycle_stats : 
  ?created_histo:histo list ->
  ?updated_histo:histo list ->
  ?merged_histo:histo list ->
  ?abandoned_histo:histo list ->
  ?created:review_count option ->
  ?opened:int32 ->
  ?abandoned:int32 ->
  ?abandoned_ratio:float ->
  ?merged:int32 ->
  ?merged_ratio:float ->
  ?self_merged:int32 ->
  ?self_merged_ratio:float ->
  ?ttm_mean:float ->
  ?ttm_variability:float ->
  ?updates_of_changes:int32 ->
  ?changes_with_tests:float ->
  ?iterations_per_change:float ->
  ?commits_per_change:float ->
  unit ->
  lifecycle_stats
(** [default_lifecycle_stats ()] is the default value for type [lifecycle_stats] *)

val default_query_response : unit -> query_response
(** [default_query_response ()] is the default value for type [query_response] *)
