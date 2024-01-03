(** issue.proto Types *)



(** {2 Types} *)

type ident = {
  uid : string;
  muid : string;
}

type issue_optional_closed_at =
  | Closed_at of TimestampTypes.timestamp

and issue = {
  id : string;
  number : int32;
  title : string;
  text : string;
  url : string;
  repository_prefix : string;
  repository_fullname : string;
  repository_shortname : string;
  author : ChangeTypes.ident option;
  created_at : TimestampTypes.timestamp option;
  updated_at : TimestampTypes.timestamp option;
  optional_closed_at : issue_optional_closed_at;
  state : string;
}

type issue_commented_event = {
  comment : string;
}

type issue_event_type =
  | Issue_created
  | Issue_commented of issue_commented_event
  | Issue_closed

and issue_event = {
  id : string;
  created_at : TimestampTypes.timestamp option;
  author : ChangeTypes.ident option;
  repository_prefix : string;
  repository_fullname : string;
  repository_shortname : string;
  number : int32;
  url : string;
  type_ : issue_event_type;
}


(** {2 Default values} *)

val default_ident : 
  ?uid:string ->
  ?muid:string ->
  unit ->
  ident
(** [default_ident ()] is the default value for type [ident] *)

val default_issue_optional_closed_at : unit -> issue_optional_closed_at
(** [default_issue_optional_closed_at ()] is the default value for type [issue_optional_closed_at] *)

val default_issue : 
  ?id:string ->
  ?number:int32 ->
  ?title:string ->
  ?text:string ->
  ?url:string ->
  ?repository_prefix:string ->
  ?repository_fullname:string ->
  ?repository_shortname:string ->
  ?author:ChangeTypes.ident option ->
  ?created_at:TimestampTypes.timestamp option ->
  ?updated_at:TimestampTypes.timestamp option ->
  ?optional_closed_at:issue_optional_closed_at ->
  ?state:string ->
  unit ->
  issue
(** [default_issue ()] is the default value for type [issue] *)

val default_issue_commented_event : 
  ?comment:string ->
  unit ->
  issue_commented_event
(** [default_issue_commented_event ()] is the default value for type [issue_commented_event] *)

val default_issue_event_type : unit -> issue_event_type
(** [default_issue_event_type ()] is the default value for type [issue_event_type] *)

val default_issue_event : 
  ?id:string ->
  ?created_at:TimestampTypes.timestamp option ->
  ?author:ChangeTypes.ident option ->
  ?repository_prefix:string ->
  ?repository_fullname:string ->
  ?repository_shortname:string ->
  ?number:int32 ->
  ?url:string ->
  ?type_:issue_event_type ->
  unit ->
  issue_event
(** [default_issue_event ()] is the default value for type [issue_event] *)
