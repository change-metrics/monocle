[@@@ocaml.warning "-27-30-39"]


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

let rec default_ident 
  ?uid:((uid:string) = "")
  ?muid:((muid:string) = "")
  () : ident  = {
  uid;
  muid;
}

let rec default_issue_optional_closed_at () : issue_optional_closed_at = Closed_at (TimestampTypes.default_timestamp)

and default_issue 
  ?id:((id:string) = "")
  ?number:((number:int32) = 0l)
  ?title:((title:string) = "")
  ?text:((text:string) = "")
  ?url:((url:string) = "")
  ?repository_prefix:((repository_prefix:string) = "")
  ?repository_fullname:((repository_fullname:string) = "")
  ?repository_shortname:((repository_shortname:string) = "")
  ?author:((author:ChangeTypes.ident option) = None)
  ?created_at:((created_at:TimestampTypes.timestamp option) = None)
  ?updated_at:((updated_at:TimestampTypes.timestamp option) = None)
  ?optional_closed_at:((optional_closed_at:issue_optional_closed_at) = Closed_at (TimestampTypes.default_timestamp))
  ?state:((state:string) = "")
  () : issue  = {
  id;
  number;
  title;
  text;
  url;
  repository_prefix;
  repository_fullname;
  repository_shortname;
  author;
  created_at;
  updated_at;
  optional_closed_at;
  state;
}

let rec default_issue_commented_event 
  ?comment:((comment:string) = "")
  () : issue_commented_event  = {
  comment;
}

let rec default_issue_event_type (): issue_event_type = Issue_created

and default_issue_event 
  ?id:((id:string) = "")
  ?created_at:((created_at:TimestampTypes.timestamp option) = None)
  ?author:((author:ChangeTypes.ident option) = None)
  ?repository_prefix:((repository_prefix:string) = "")
  ?repository_fullname:((repository_fullname:string) = "")
  ?repository_shortname:((repository_shortname:string) = "")
  ?number:((number:int32) = 0l)
  ?url:((url:string) = "")
  ?type_:((type_:issue_event_type) = Issue_created)
  () : issue_event  = {
  id;
  created_at;
  author;
  repository_prefix;
  repository_fullname;
  repository_shortname;
  number;
  url;
  type_;
}
