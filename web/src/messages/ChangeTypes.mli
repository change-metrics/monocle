(** change.proto Types *)



(** {2 Types} *)

type ident = {
  uid : string;
  muid : string;
  groups : string list;
}

type changed_file = {
  additions : int32;
  deletions : int32;
  path : string;
}

type changed_file_path = {
  path : string;
}

type commit = {
  sha : string;
  author : ident option;
  committer : ident option;
  authored_at : TimestampTypes.timestamp option;
  committed_at : TimestampTypes.timestamp option;
  additions : int32;
  deletions : int32;
  title : string;
}

type change_change_state =
  | Open 
  | Merged 
  | Closed 

type change_optional_merged_by =
  | Merged_by of ident

and change_optional_merged_at =
  | Merged_at of TimestampTypes.timestamp

and change_optional_closed_at =
  | Closed_at of TimestampTypes.timestamp

and change_optional_duration =
  | Duration of int32

and change_optional_self_merged =
  | Self_merged of bool

and change_optional_merged_commit_sha =
  | Merged_commit_sha of string

and change = {
  id : string;
  number : int32;
  change_id : string;
  title : string;
  text : string;
  url : string;
  commit_count : int32;
  additions : int32;
  deletions : int32;
  changed_files_count : int32;
  changed_files : changed_file list;
  commits : commit list;
  repository_prefix : string;
  repository_fullname : string;
  repository_shortname : string;
  author : ident option;
  optional_merged_by : change_optional_merged_by;
  branch : string;
  target_branch : string;
  created_at : TimestampTypes.timestamp option;
  optional_merged_at : change_optional_merged_at;
  updated_at : TimestampTypes.timestamp option;
  optional_closed_at : change_optional_closed_at;
  state : change_change_state;
  optional_duration : change_optional_duration;
  mergeable : string;
  labels : string list;
  assignees : ident list;
  approvals : string list;
  draft : bool;
  optional_self_merged : change_optional_self_merged;
  optional_merged_commit_sha : change_optional_merged_commit_sha;
}

type change_reviewed_event = {
  approvals : string list;
}

type change_event_type =
  | Change_created
  | Change_commented
  | Change_abandoned
  | Change_reviewed of change_reviewed_event
  | Change_commit_force_pushed
  | Change_commit_pushed
  | Change_merged

and change_event_optional_duration =
  | Duration of int32

and change_event_optional_merged_commit_sha =
  | Merged_commit_sha of string

and change_event = {
  id : string;
  created_at : TimestampTypes.timestamp option;
  author : ident option;
  repository_prefix : string;
  repository_fullname : string;
  repository_shortname : string;
  branch : string;
  target_branch : string;
  number : int32;
  change_id : string;
  url : string;
  on_author : ident option;
  on_created_at : TimestampTypes.timestamp option;
  changed_files : changed_file_path list;
  type_ : change_event_type;
  labels : string list;
  optional_duration : change_event_optional_duration;
  draft : bool;
  optional_merged_commit_sha : change_event_optional_merged_commit_sha;
}


(** {2 Default values} *)

val default_ident : 
  ?uid:string ->
  ?muid:string ->
  ?groups:string list ->
  unit ->
  ident
(** [default_ident ()] is the default value for type [ident] *)

val default_changed_file : 
  ?additions:int32 ->
  ?deletions:int32 ->
  ?path:string ->
  unit ->
  changed_file
(** [default_changed_file ()] is the default value for type [changed_file] *)

val default_changed_file_path : 
  ?path:string ->
  unit ->
  changed_file_path
(** [default_changed_file_path ()] is the default value for type [changed_file_path] *)

val default_commit : 
  ?sha:string ->
  ?author:ident option ->
  ?committer:ident option ->
  ?authored_at:TimestampTypes.timestamp option ->
  ?committed_at:TimestampTypes.timestamp option ->
  ?additions:int32 ->
  ?deletions:int32 ->
  ?title:string ->
  unit ->
  commit
(** [default_commit ()] is the default value for type [commit] *)

val default_change_change_state : unit -> change_change_state
(** [default_change_change_state ()] is the default value for type [change_change_state] *)

val default_change_optional_merged_by : unit -> change_optional_merged_by
(** [default_change_optional_merged_by ()] is the default value for type [change_optional_merged_by] *)

val default_change_optional_merged_at : unit -> change_optional_merged_at
(** [default_change_optional_merged_at ()] is the default value for type [change_optional_merged_at] *)

val default_change_optional_closed_at : unit -> change_optional_closed_at
(** [default_change_optional_closed_at ()] is the default value for type [change_optional_closed_at] *)

val default_change_optional_duration : unit -> change_optional_duration
(** [default_change_optional_duration ()] is the default value for type [change_optional_duration] *)

val default_change_optional_self_merged : unit -> change_optional_self_merged
(** [default_change_optional_self_merged ()] is the default value for type [change_optional_self_merged] *)

val default_change_optional_merged_commit_sha : unit -> change_optional_merged_commit_sha
(** [default_change_optional_merged_commit_sha ()] is the default value for type [change_optional_merged_commit_sha] *)

val default_change : 
  ?id:string ->
  ?number:int32 ->
  ?change_id:string ->
  ?title:string ->
  ?text:string ->
  ?url:string ->
  ?commit_count:int32 ->
  ?additions:int32 ->
  ?deletions:int32 ->
  ?changed_files_count:int32 ->
  ?changed_files:changed_file list ->
  ?commits:commit list ->
  ?repository_prefix:string ->
  ?repository_fullname:string ->
  ?repository_shortname:string ->
  ?author:ident option ->
  ?optional_merged_by:change_optional_merged_by ->
  ?branch:string ->
  ?target_branch:string ->
  ?created_at:TimestampTypes.timestamp option ->
  ?optional_merged_at:change_optional_merged_at ->
  ?updated_at:TimestampTypes.timestamp option ->
  ?optional_closed_at:change_optional_closed_at ->
  ?state:change_change_state ->
  ?optional_duration:change_optional_duration ->
  ?mergeable:string ->
  ?labels:string list ->
  ?assignees:ident list ->
  ?approvals:string list ->
  ?draft:bool ->
  ?optional_self_merged:change_optional_self_merged ->
  ?optional_merged_commit_sha:change_optional_merged_commit_sha ->
  unit ->
  change
(** [default_change ()] is the default value for type [change] *)

val default_change_reviewed_event : 
  ?approvals:string list ->
  unit ->
  change_reviewed_event
(** [default_change_reviewed_event ()] is the default value for type [change_reviewed_event] *)

val default_change_event_type : unit -> change_event_type
(** [default_change_event_type ()] is the default value for type [change_event_type] *)

val default_change_event_optional_duration : unit -> change_event_optional_duration
(** [default_change_event_optional_duration ()] is the default value for type [change_event_optional_duration] *)

val default_change_event_optional_merged_commit_sha : unit -> change_event_optional_merged_commit_sha
(** [default_change_event_optional_merged_commit_sha ()] is the default value for type [change_event_optional_merged_commit_sha] *)

val default_change_event : 
  ?id:string ->
  ?created_at:TimestampTypes.timestamp option ->
  ?author:ident option ->
  ?repository_prefix:string ->
  ?repository_fullname:string ->
  ?repository_shortname:string ->
  ?branch:string ->
  ?target_branch:string ->
  ?number:int32 ->
  ?change_id:string ->
  ?url:string ->
  ?on_author:ident option ->
  ?on_created_at:TimestampTypes.timestamp option ->
  ?changed_files:changed_file_path list ->
  ?type_:change_event_type ->
  ?labels:string list ->
  ?optional_duration:change_event_optional_duration ->
  ?draft:bool ->
  ?optional_merged_commit_sha:change_event_optional_merged_commit_sha ->
  unit ->
  change_event
(** [default_change_event ()] is the default value for type [change_event] *)
