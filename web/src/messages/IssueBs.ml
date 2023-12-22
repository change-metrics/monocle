[@@@ocaml.warning "-27-30-39"]

type ident_mutable = {
  mutable uid : string;
  mutable muid : string;
}

let default_ident_mutable () : ident_mutable = {
  uid = "";
  muid = "";
}

type issue_mutable = {
  mutable id : string;
  mutable number : int32;
  mutable title : string;
  mutable text : string;
  mutable url : string;
  mutable repository_prefix : string;
  mutable repository_fullname : string;
  mutable repository_shortname : string;
  mutable author : ChangeTypes.ident option;
  mutable created_at : TimestampTypes.timestamp option;
  mutable updated_at : TimestampTypes.timestamp option;
  mutable optional_closed_at : IssueTypes.issue_optional_closed_at;
  mutable state : string;
}

let default_issue_mutable () : issue_mutable = {
  id = "";
  number = 0l;
  title = "";
  text = "";
  url = "";
  repository_prefix = "";
  repository_fullname = "";
  repository_shortname = "";
  author = None;
  created_at = None;
  updated_at = None;
  optional_closed_at = IssueTypes.Closed_at (TimestampTypes.default_timestamp);
  state = "";
}

type issue_commented_event_mutable = {
  mutable comment : string;
}

let default_issue_commented_event_mutable () : issue_commented_event_mutable = {
  comment = "";
}

type issue_event_mutable = {
  mutable id : string;
  mutable created_at : TimestampTypes.timestamp option;
  mutable author : ChangeTypes.ident option;
  mutable repository_prefix : string;
  mutable repository_fullname : string;
  mutable repository_shortname : string;
  mutable number : int32;
  mutable url : string;
  mutable type_ : IssueTypes.issue_event_type;
}

let default_issue_event_mutable () : issue_event_mutable = {
  id = "";
  created_at = None;
  author = None;
  repository_prefix = "";
  repository_fullname = "";
  repository_shortname = "";
  number = 0l;
  url = "";
  type_ = IssueTypes.Issue_created;
}


let rec decode_ident json =
  let v = default_ident_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "uid" -> 
      let json = Js.Dict.unsafeGet json "uid" in
      v.uid <- Pbrt_bs.string json "ident" "uid"
    | "muid" -> 
      let json = Js.Dict.unsafeGet json "muid" in
      v.muid <- Pbrt_bs.string json "ident" "muid"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    IssueTypes.uid = v.uid;
    IssueTypes.muid = v.muid;
  } : IssueTypes.ident)

let rec decode_issue_optional_closed_at json =
  let keys = Js.Dict.keys json in
  let rec loop = function 
    | -1 -> Pbrt_bs.E.malformed_variant "issue_optional_closed_at"
    | i -> 
      begin match Array.unsafe_get keys i with
      | "closed_at" -> 
        let json = Js.Dict.unsafeGet json "closed_at" in
        (IssueTypes.Closed_at ((TimestampBs.decode_timestamp (Pbrt_bs.string json "issue_optional_closed_at" "Closed_at"))) : IssueTypes.issue_optional_closed_at)
      
      | _ -> loop (i - 1)
      end
  in
  loop (Array.length keys - 1)

and decode_issue json =
  let v = default_issue_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "id" -> 
      let json = Js.Dict.unsafeGet json "id" in
      v.id <- Pbrt_bs.string json "issue" "id"
    | "number" -> 
      let json = Js.Dict.unsafeGet json "number" in
      v.number <- Pbrt_bs.int32 json "issue" "number"
    | "title" -> 
      let json = Js.Dict.unsafeGet json "title" in
      v.title <- Pbrt_bs.string json "issue" "title"
    | "text" -> 
      let json = Js.Dict.unsafeGet json "text" in
      v.text <- Pbrt_bs.string json "issue" "text"
    | "url" -> 
      let json = Js.Dict.unsafeGet json "url" in
      v.url <- Pbrt_bs.string json "issue" "url"
    | "repository_prefix" -> 
      let json = Js.Dict.unsafeGet json "repository_prefix" in
      v.repository_prefix <- Pbrt_bs.string json "issue" "repository_prefix"
    | "repository_fullname" -> 
      let json = Js.Dict.unsafeGet json "repository_fullname" in
      v.repository_fullname <- Pbrt_bs.string json "issue" "repository_fullname"
    | "repository_shortname" -> 
      let json = Js.Dict.unsafeGet json "repository_shortname" in
      v.repository_shortname <- Pbrt_bs.string json "issue" "repository_shortname"
    | "author" -> 
      let json = Js.Dict.unsafeGet json "author" in
      v.author <- Some ((ChangeBs.decode_ident (Pbrt_bs.object_ json "issue" "author")))
    | "created_at" -> 
      let json = Js.Dict.unsafeGet json "created_at" in
      v.created_at <- Some ((TimestampBs.decode_timestamp (Pbrt_bs.string json "issue" "created_at")))
    | "updated_at" -> 
      let json = Js.Dict.unsafeGet json "updated_at" in
      v.updated_at <- Some ((TimestampBs.decode_timestamp (Pbrt_bs.string json "issue" "updated_at")))
    | "closed_at" -> 
      let json = Js.Dict.unsafeGet json "closed_at" in
      v.optional_closed_at <- Closed_at ((TimestampBs.decode_timestamp (Pbrt_bs.string json "issue" "optional_closed_at")))
    | "state" -> 
      let json = Js.Dict.unsafeGet json "state" in
      v.state <- Pbrt_bs.string json "issue" "state"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    IssueTypes.id = v.id;
    IssueTypes.number = v.number;
    IssueTypes.title = v.title;
    IssueTypes.text = v.text;
    IssueTypes.url = v.url;
    IssueTypes.repository_prefix = v.repository_prefix;
    IssueTypes.repository_fullname = v.repository_fullname;
    IssueTypes.repository_shortname = v.repository_shortname;
    IssueTypes.author = v.author;
    IssueTypes.created_at = v.created_at;
    IssueTypes.updated_at = v.updated_at;
    IssueTypes.optional_closed_at = v.optional_closed_at;
    IssueTypes.state = v.state;
  } : IssueTypes.issue)

let rec decode_issue_commented_event json =
  let v = default_issue_commented_event_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "comment" -> 
      let json = Js.Dict.unsafeGet json "comment" in
      v.comment <- Pbrt_bs.string json "issue_commented_event" "comment"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    IssueTypes.comment = v.comment;
  } : IssueTypes.issue_commented_event)

let rec decode_issue_event_type json =
  let keys = Js.Dict.keys json in
  let rec loop = function 
    | -1 -> Pbrt_bs.E.malformed_variant "issue_event_type"
    | i -> 
      begin match Array.unsafe_get keys i with
      | "issue_created" -> (IssueTypes.Issue_created : IssueTypes.issue_event_type)
      | "issue_commented" -> 
        let json = Js.Dict.unsafeGet json "issue_commented" in
        (IssueTypes.Issue_commented ((decode_issue_commented_event (Pbrt_bs.object_ json "issue_event_type" "Issue_commented"))) : IssueTypes.issue_event_type)
      | "issue_closed" -> (IssueTypes.Issue_closed : IssueTypes.issue_event_type)
      
      | _ -> loop (i - 1)
      end
  in
  loop (Array.length keys - 1)

and decode_issue_event json =
  let v = default_issue_event_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "id" -> 
      let json = Js.Dict.unsafeGet json "id" in
      v.id <- Pbrt_bs.string json "issue_event" "id"
    | "created_at" -> 
      let json = Js.Dict.unsafeGet json "created_at" in
      v.created_at <- Some ((TimestampBs.decode_timestamp (Pbrt_bs.string json "issue_event" "created_at")))
    | "author" -> 
      let json = Js.Dict.unsafeGet json "author" in
      v.author <- Some ((ChangeBs.decode_ident (Pbrt_bs.object_ json "issue_event" "author")))
    | "repository_prefix" -> 
      let json = Js.Dict.unsafeGet json "repository_prefix" in
      v.repository_prefix <- Pbrt_bs.string json "issue_event" "repository_prefix"
    | "repository_fullname" -> 
      let json = Js.Dict.unsafeGet json "repository_fullname" in
      v.repository_fullname <- Pbrt_bs.string json "issue_event" "repository_fullname"
    | "repository_shortname" -> 
      let json = Js.Dict.unsafeGet json "repository_shortname" in
      v.repository_shortname <- Pbrt_bs.string json "issue_event" "repository_shortname"
    | "number" -> 
      let json = Js.Dict.unsafeGet json "number" in
      v.number <- Pbrt_bs.int32 json "issue_event" "number"
    | "url" -> 
      let json = Js.Dict.unsafeGet json "url" in
      v.url <- Pbrt_bs.string json "issue_event" "url"
    | "issue_created" -> v.type_ <- Issue_created
    | "issue_commented" -> 
      let json = Js.Dict.unsafeGet json "issue_commented" in
      v.type_ <- Issue_commented ((decode_issue_commented_event (Pbrt_bs.object_ json "issue_event" "type_")))
    | "issue_closed" -> v.type_ <- Issue_closed
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    IssueTypes.id = v.id;
    IssueTypes.created_at = v.created_at;
    IssueTypes.author = v.author;
    IssueTypes.repository_prefix = v.repository_prefix;
    IssueTypes.repository_fullname = v.repository_fullname;
    IssueTypes.repository_shortname = v.repository_shortname;
    IssueTypes.number = v.number;
    IssueTypes.url = v.url;
    IssueTypes.type_ = v.type_;
  } : IssueTypes.issue_event)

let rec encode_ident (v:IssueTypes.ident) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "uid" (Js.Json.string v.IssueTypes.uid);
  Js.Dict.set json "muid" (Js.Json.string v.IssueTypes.muid);
  json

let rec encode_issue_optional_closed_at (v:IssueTypes.issue_optional_closed_at) = 
  let json = Js.Dict.empty () in
  begin match v with
  | IssueTypes.Closed_at v ->
    begin (* closed_at field *)
      let json' = TimestampBs.encode_timestamp v in
      Js.Dict.set json "closed_at" (Js.Json.string json');
    end;
  end;
  json

and encode_issue (v:IssueTypes.issue) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "id" (Js.Json.string v.IssueTypes.id);
  Js.Dict.set json "number" (Js.Json.number (Int32.to_float v.IssueTypes.number));
  Js.Dict.set json "title" (Js.Json.string v.IssueTypes.title);
  Js.Dict.set json "text" (Js.Json.string v.IssueTypes.text);
  Js.Dict.set json "url" (Js.Json.string v.IssueTypes.url);
  Js.Dict.set json "repository_prefix" (Js.Json.string v.IssueTypes.repository_prefix);
  Js.Dict.set json "repository_fullname" (Js.Json.string v.IssueTypes.repository_fullname);
  Js.Dict.set json "repository_shortname" (Js.Json.string v.IssueTypes.repository_shortname);
  begin match v.IssueTypes.author with
  | None -> ()
  | Some v ->
    begin (* author field *)
      let json' = ChangeBs.encode_ident v in
      Js.Dict.set json "author" (Js.Json.object_ json');
    end;
  end;
  begin match v.IssueTypes.created_at with
  | None -> ()
  | Some v ->
    begin (* created_at field *)
      let json' = TimestampBs.encode_timestamp v in
      Js.Dict.set json "created_at" (Js.Json.string json');
    end;
  end;
  begin match v.IssueTypes.updated_at with
  | None -> ()
  | Some v ->
    begin (* updated_at field *)
      let json' = TimestampBs.encode_timestamp v in
      Js.Dict.set json "updated_at" (Js.Json.string json');
    end;
  end;
  begin match v.IssueTypes.optional_closed_at with
    | Closed_at v ->
      begin (* closed_at field *)
        let json' = TimestampBs.encode_timestamp v in
        Js.Dict.set json "closed_at" (Js.Json.string json');
      end;
  end; (* match v.optional_closed_at *)
  Js.Dict.set json "state" (Js.Json.string v.IssueTypes.state);
  json

let rec encode_issue_commented_event (v:IssueTypes.issue_commented_event) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "comment" (Js.Json.string v.IssueTypes.comment);
  json

let rec encode_issue_event_type (v:IssueTypes.issue_event_type) = 
  let json = Js.Dict.empty () in
  begin match v with
  | IssueTypes.Issue_created ->
    Js.Dict.set json "issue_created" Js.Json.null
  | IssueTypes.Issue_commented v ->
    begin (* issueCommented field *)
      let json' = encode_issue_commented_event v in
      Js.Dict.set json "issue_commented" (Js.Json.object_ json');
    end;
  | IssueTypes.Issue_closed ->
    Js.Dict.set json "issue_closed" Js.Json.null
  end;
  json

and encode_issue_event (v:IssueTypes.issue_event) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "id" (Js.Json.string v.IssueTypes.id);
  begin match v.IssueTypes.created_at with
  | None -> ()
  | Some v ->
    begin (* created_at field *)
      let json' = TimestampBs.encode_timestamp v in
      Js.Dict.set json "created_at" (Js.Json.string json');
    end;
  end;
  begin match v.IssueTypes.author with
  | None -> ()
  | Some v ->
    begin (* author field *)
      let json' = ChangeBs.encode_ident v in
      Js.Dict.set json "author" (Js.Json.object_ json');
    end;
  end;
  Js.Dict.set json "repository_prefix" (Js.Json.string v.IssueTypes.repository_prefix);
  Js.Dict.set json "repository_fullname" (Js.Json.string v.IssueTypes.repository_fullname);
  Js.Dict.set json "repository_shortname" (Js.Json.string v.IssueTypes.repository_shortname);
  Js.Dict.set json "number" (Js.Json.number (Int32.to_float v.IssueTypes.number));
  Js.Dict.set json "url" (Js.Json.string v.IssueTypes.url);
  begin match v.IssueTypes.type_ with
    | Issue_created ->
      Js.Dict.set json "issue_created" Js.Json.null
    | Issue_commented v ->
      begin (* issueCommented field *)
        let json' = encode_issue_commented_event v in
        Js.Dict.set json "issue_commented" (Js.Json.object_ json');
      end;
    | Issue_closed ->
      Js.Dict.set json "issue_closed" Js.Json.null
  end; (* match v.type_ *)
  json
