[@@@ocaml.warning "-27-30-39"]


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

let rec default_ident 
  ?uid:((uid:string) = "")
  ?muid:((muid:string) = "")
  ?groups:((groups:string list) = [])
  () : ident  = {
  uid;
  muid;
  groups;
}

let rec default_changed_file 
  ?additions:((additions:int32) = 0l)
  ?deletions:((deletions:int32) = 0l)
  ?path:((path:string) = "")
  () : changed_file  = {
  additions;
  deletions;
  path;
}

let rec default_changed_file_path 
  ?path:((path:string) = "")
  () : changed_file_path  = {
  path;
}

let rec default_commit 
  ?sha:((sha:string) = "")
  ?author:((author:ident option) = None)
  ?committer:((committer:ident option) = None)
  ?authored_at:((authored_at:TimestampTypes.timestamp option) = None)
  ?committed_at:((committed_at:TimestampTypes.timestamp option) = None)
  ?additions:((additions:int32) = 0l)
  ?deletions:((deletions:int32) = 0l)
  ?title:((title:string) = "")
  () : commit  = {
  sha;
  author;
  committer;
  authored_at;
  committed_at;
  additions;
  deletions;
  title;
}

let rec default_change_change_state () = (Open:change_change_state)

let rec default_change_optional_merged_by () : change_optional_merged_by = Merged_by (default_ident ())

and default_change_optional_merged_at () : change_optional_merged_at = Merged_at (TimestampTypes.default_timestamp)

and default_change_optional_closed_at () : change_optional_closed_at = Closed_at (TimestampTypes.default_timestamp)

and default_change_optional_duration () : change_optional_duration = Duration (0l)

and default_change_optional_self_merged () : change_optional_self_merged = Self_merged (false)

and default_change_optional_merged_commit_sha () : change_optional_merged_commit_sha = Merged_commit_sha ("")

and default_change 
  ?id:((id:string) = "")
  ?number:((number:int32) = 0l)
  ?change_id:((change_id:string) = "")
  ?title:((title:string) = "")
  ?text:((text:string) = "")
  ?url:((url:string) = "")
  ?commit_count:((commit_count:int32) = 0l)
  ?additions:((additions:int32) = 0l)
  ?deletions:((deletions:int32) = 0l)
  ?changed_files_count:((changed_files_count:int32) = 0l)
  ?changed_files:((changed_files:changed_file list) = [])
  ?commits:((commits:commit list) = [])
  ?repository_prefix:((repository_prefix:string) = "")
  ?repository_fullname:((repository_fullname:string) = "")
  ?repository_shortname:((repository_shortname:string) = "")
  ?author:((author:ident option) = None)
  ?optional_merged_by:((optional_merged_by:change_optional_merged_by) = Merged_by (default_ident ()))
  ?branch:((branch:string) = "")
  ?target_branch:((target_branch:string) = "")
  ?created_at:((created_at:TimestampTypes.timestamp option) = None)
  ?optional_merged_at:((optional_merged_at:change_optional_merged_at) = Merged_at (TimestampTypes.default_timestamp))
  ?updated_at:((updated_at:TimestampTypes.timestamp option) = None)
  ?optional_closed_at:((optional_closed_at:change_optional_closed_at) = Closed_at (TimestampTypes.default_timestamp))
  ?state:((state:change_change_state) = default_change_change_state ())
  ?optional_duration:((optional_duration:change_optional_duration) = Duration (0l))
  ?mergeable:((mergeable:string) = "")
  ?labels:((labels:string list) = [])
  ?assignees:((assignees:ident list) = [])
  ?approvals:((approvals:string list) = [])
  ?draft:((draft:bool) = false)
  ?optional_self_merged:((optional_self_merged:change_optional_self_merged) = Self_merged (false))
  ?optional_merged_commit_sha:((optional_merged_commit_sha:change_optional_merged_commit_sha) = Merged_commit_sha (""))
  () : change  = {
  id;
  number;
  change_id;
  title;
  text;
  url;
  commit_count;
  additions;
  deletions;
  changed_files_count;
  changed_files;
  commits;
  repository_prefix;
  repository_fullname;
  repository_shortname;
  author;
  optional_merged_by;
  branch;
  target_branch;
  created_at;
  optional_merged_at;
  updated_at;
  optional_closed_at;
  state;
  optional_duration;
  mergeable;
  labels;
  assignees;
  approvals;
  draft;
  optional_self_merged;
  optional_merged_commit_sha;
}

let rec default_change_reviewed_event 
  ?approvals:((approvals:string list) = [])
  () : change_reviewed_event  = {
  approvals;
}

let rec default_change_event_type (): change_event_type = Change_created

and default_change_event_optional_duration () : change_event_optional_duration = Duration (0l)

and default_change_event_optional_merged_commit_sha () : change_event_optional_merged_commit_sha = Merged_commit_sha ("")

and default_change_event 
  ?id:((id:string) = "")
  ?created_at:((created_at:TimestampTypes.timestamp option) = None)
  ?author:((author:ident option) = None)
  ?repository_prefix:((repository_prefix:string) = "")
  ?repository_fullname:((repository_fullname:string) = "")
  ?repository_shortname:((repository_shortname:string) = "")
  ?branch:((branch:string) = "")
  ?target_branch:((target_branch:string) = "")
  ?number:((number:int32) = 0l)
  ?change_id:((change_id:string) = "")
  ?url:((url:string) = "")
  ?on_author:((on_author:ident option) = None)
  ?on_created_at:((on_created_at:TimestampTypes.timestamp option) = None)
  ?changed_files:((changed_files:changed_file_path list) = [])
  ?type_:((type_:change_event_type) = Change_created)
  ?labels:((labels:string list) = [])
  ?optional_duration:((optional_duration:change_event_optional_duration) = Duration (0l))
  ?draft:((draft:bool) = false)
  ?optional_merged_commit_sha:((optional_merged_commit_sha:change_event_optional_merged_commit_sha) = Merged_commit_sha (""))
  () : change_event  = {
  id;
  created_at;
  author;
  repository_prefix;
  repository_fullname;
  repository_shortname;
  branch;
  target_branch;
  number;
  change_id;
  url;
  on_author;
  on_created_at;
  changed_files;
  type_;
  labels;
  optional_duration;
  draft;
  optional_merged_commit_sha;
}
