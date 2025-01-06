[@@@ocaml.warning "-27-30-39"]

type ident_mutable = {
  mutable uid : string;
  mutable muid : string;
  mutable groups : string list;
}

let default_ident_mutable () : ident_mutable = {
  uid = "";
  muid = "";
  groups = [];
}

type changed_file_mutable = {
  mutable additions : int32;
  mutable deletions : int32;
  mutable path : string;
}

let default_changed_file_mutable () : changed_file_mutable = {
  additions = 0l;
  deletions = 0l;
  path = "";
}

type changed_file_path_mutable = {
  mutable path : string;
}

let default_changed_file_path_mutable () : changed_file_path_mutable = {
  path = "";
}

type commit_mutable = {
  mutable sha : string;
  mutable author : ChangeTypes.ident option;
  mutable committer : ChangeTypes.ident option;
  mutable authored_at : TimestampTypes.timestamp option;
  mutable committed_at : TimestampTypes.timestamp option;
  mutable additions : int32;
  mutable deletions : int32;
  mutable title : string;
}

let default_commit_mutable () : commit_mutable = {
  sha = "";
  author = None;
  committer = None;
  authored_at = None;
  committed_at = None;
  additions = 0l;
  deletions = 0l;
  title = "";
}

type change_mutable = {
  mutable id : string;
  mutable number : int32;
  mutable change_id : string;
  mutable title : string;
  mutable text : string;
  mutable url : string;
  mutable commit_count : int32;
  mutable additions : int32;
  mutable deletions : int32;
  mutable changed_files_count : int32;
  mutable changed_files : ChangeTypes.changed_file list;
  mutable commits : ChangeTypes.commit list;
  mutable repository_prefix : string;
  mutable repository_fullname : string;
  mutable repository_shortname : string;
  mutable author : ChangeTypes.ident option;
  mutable optional_merged_by : ChangeTypes.change_optional_merged_by;
  mutable branch : string;
  mutable target_branch : string;
  mutable created_at : TimestampTypes.timestamp option;
  mutable optional_merged_at : ChangeTypes.change_optional_merged_at;
  mutable updated_at : TimestampTypes.timestamp option;
  mutable optional_closed_at : ChangeTypes.change_optional_closed_at;
  mutable state : ChangeTypes.change_change_state;
  mutable optional_duration : ChangeTypes.change_optional_duration;
  mutable mergeable : string;
  mutable labels : string list;
  mutable assignees : ChangeTypes.ident list;
  mutable approvals : string list;
  mutable draft : bool;
  mutable optional_self_merged : ChangeTypes.change_optional_self_merged;
  mutable optional_merged_commit_sha : ChangeTypes.change_optional_merged_commit_sha;
}

let default_change_mutable () : change_mutable = {
  id = "";
  number = 0l;
  change_id = "";
  title = "";
  text = "";
  url = "";
  commit_count = 0l;
  additions = 0l;
  deletions = 0l;
  changed_files_count = 0l;
  changed_files = [];
  commits = [];
  repository_prefix = "";
  repository_fullname = "";
  repository_shortname = "";
  author = None;
  optional_merged_by = ChangeTypes.Merged_by (ChangeTypes.default_ident ());
  branch = "";
  target_branch = "";
  created_at = None;
  optional_merged_at = ChangeTypes.Merged_at (TimestampTypes.default_timestamp);
  updated_at = None;
  optional_closed_at = ChangeTypes.Closed_at (TimestampTypes.default_timestamp);
  state = ChangeTypes.default_change_change_state ();
  optional_duration = ChangeTypes.Duration (0l);
  mergeable = "";
  labels = [];
  assignees = [];
  approvals = [];
  draft = false;
  optional_self_merged = ChangeTypes.Self_merged (false);
  optional_merged_commit_sha = ChangeTypes.Merged_commit_sha ("");
}

type change_reviewed_event_mutable = {
  mutable approvals : string list;
}

let default_change_reviewed_event_mutable () : change_reviewed_event_mutable = {
  approvals = [];
}

type change_event_mutable = {
  mutable id : string;
  mutable created_at : TimestampTypes.timestamp option;
  mutable author : ChangeTypes.ident option;
  mutable repository_prefix : string;
  mutable repository_fullname : string;
  mutable repository_shortname : string;
  mutable branch : string;
  mutable target_branch : string;
  mutable number : int32;
  mutable change_id : string;
  mutable url : string;
  mutable on_author : ChangeTypes.ident option;
  mutable on_created_at : TimestampTypes.timestamp option;
  mutable changed_files : ChangeTypes.changed_file_path list;
  mutable type_ : ChangeTypes.change_event_type;
  mutable labels : string list;
  mutable optional_duration : ChangeTypes.change_event_optional_duration;
  mutable draft : bool;
  mutable optional_merged_commit_sha : ChangeTypes.change_event_optional_merged_commit_sha;
}

let default_change_event_mutable () : change_event_mutable = {
  id = "";
  created_at = None;
  author = None;
  repository_prefix = "";
  repository_fullname = "";
  repository_shortname = "";
  branch = "";
  target_branch = "";
  number = 0l;
  change_id = "";
  url = "";
  on_author = None;
  on_created_at = None;
  changed_files = [];
  type_ = ChangeTypes.Change_created;
  labels = [];
  optional_duration = ChangeTypes.Duration (0l);
  draft = false;
  optional_merged_commit_sha = ChangeTypes.Merged_commit_sha ("");
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
    | "groups" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "groups" in 
        Pbrt_bs.array_ a "ident" "groups"
      in
      v.groups <- Array.map (fun json -> 
        Pbrt_bs.string json "ident" "groups"
      ) a |> Array.to_list;
    end
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    ChangeTypes.uid = v.uid;
    ChangeTypes.muid = v.muid;
    ChangeTypes.groups = v.groups;
  } : ChangeTypes.ident)

let rec decode_changed_file json =
  let v = default_changed_file_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "additions" -> 
      let json = Js.Dict.unsafeGet json "additions" in
      v.additions <- Pbrt_bs.int32 json "changed_file" "additions"
    | "deletions" -> 
      let json = Js.Dict.unsafeGet json "deletions" in
      v.deletions <- Pbrt_bs.int32 json "changed_file" "deletions"
    | "path" -> 
      let json = Js.Dict.unsafeGet json "path" in
      v.path <- Pbrt_bs.string json "changed_file" "path"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    ChangeTypes.additions = v.additions;
    ChangeTypes.deletions = v.deletions;
    ChangeTypes.path = v.path;
  } : ChangeTypes.changed_file)

let rec decode_changed_file_path json =
  let v = default_changed_file_path_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "path" -> 
      let json = Js.Dict.unsafeGet json "path" in
      v.path <- Pbrt_bs.string json "changed_file_path" "path"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    ChangeTypes.path = v.path;
  } : ChangeTypes.changed_file_path)

let rec decode_commit json =
  let v = default_commit_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "sha" -> 
      let json = Js.Dict.unsafeGet json "sha" in
      v.sha <- Pbrt_bs.string json "commit" "sha"
    | "author" -> 
      let json = Js.Dict.unsafeGet json "author" in
      v.author <- Some ((decode_ident (Pbrt_bs.object_ json "commit" "author")))
    | "committer" -> 
      let json = Js.Dict.unsafeGet json "committer" in
      v.committer <- Some ((decode_ident (Pbrt_bs.object_ json "commit" "committer")))
    | "authored_at" -> 
      let json = Js.Dict.unsafeGet json "authored_at" in
      v.authored_at <- Some ((TimestampBs.decode_timestamp (Pbrt_bs.string json "commit" "authored_at")))
    | "committed_at" -> 
      let json = Js.Dict.unsafeGet json "committed_at" in
      v.committed_at <- Some ((TimestampBs.decode_timestamp (Pbrt_bs.string json "commit" "committed_at")))
    | "additions" -> 
      let json = Js.Dict.unsafeGet json "additions" in
      v.additions <- Pbrt_bs.int32 json "commit" "additions"
    | "deletions" -> 
      let json = Js.Dict.unsafeGet json "deletions" in
      v.deletions <- Pbrt_bs.int32 json "commit" "deletions"
    | "title" -> 
      let json = Js.Dict.unsafeGet json "title" in
      v.title <- Pbrt_bs.string json "commit" "title"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    ChangeTypes.sha = v.sha;
    ChangeTypes.author = v.author;
    ChangeTypes.committer = v.committer;
    ChangeTypes.authored_at = v.authored_at;
    ChangeTypes.committed_at = v.committed_at;
    ChangeTypes.additions = v.additions;
    ChangeTypes.deletions = v.deletions;
    ChangeTypes.title = v.title;
  } : ChangeTypes.commit)

let rec decode_change_change_state (json:Js.Json.t) =
  match Pbrt_bs.string json "change_change_state" "value" with
  | "Open" -> (ChangeTypes.Open : ChangeTypes.change_change_state)
  | "Merged" -> (ChangeTypes.Merged : ChangeTypes.change_change_state)
  | "Closed" -> (ChangeTypes.Closed : ChangeTypes.change_change_state)
  | "" -> ChangeTypes.Open
  | _ -> Pbrt_bs.E.malformed_variant "change_change_state"

let rec decode_change_optional_merged_by json =
  let keys = Js.Dict.keys json in
  let rec loop = function 
    | -1 -> Pbrt_bs.E.malformed_variant "change_optional_merged_by"
    | i -> 
      begin match Array.unsafe_get keys i with
      | "merged_by" -> 
        let json = Js.Dict.unsafeGet json "merged_by" in
        (ChangeTypes.Merged_by ((decode_ident (Pbrt_bs.object_ json "change_optional_merged_by" "Merged_by"))) : ChangeTypes.change_optional_merged_by)
      
      | _ -> loop (i - 1)
      end
  in
  loop (Array.length keys - 1)

and decode_change_optional_merged_at json =
  let keys = Js.Dict.keys json in
  let rec loop = function 
    | -1 -> Pbrt_bs.E.malformed_variant "change_optional_merged_at"
    | i -> 
      begin match Array.unsafe_get keys i with
      | "merged_at" -> 
        let json = Js.Dict.unsafeGet json "merged_at" in
        (ChangeTypes.Merged_at ((TimestampBs.decode_timestamp (Pbrt_bs.string json "change_optional_merged_at" "Merged_at"))) : ChangeTypes.change_optional_merged_at)
      
      | _ -> loop (i - 1)
      end
  in
  loop (Array.length keys - 1)

and decode_change_optional_closed_at json =
  let keys = Js.Dict.keys json in
  let rec loop = function 
    | -1 -> Pbrt_bs.E.malformed_variant "change_optional_closed_at"
    | i -> 
      begin match Array.unsafe_get keys i with
      | "closed_at" -> 
        let json = Js.Dict.unsafeGet json "closed_at" in
        (ChangeTypes.Closed_at ((TimestampBs.decode_timestamp (Pbrt_bs.string json "change_optional_closed_at" "Closed_at"))) : ChangeTypes.change_optional_closed_at)
      
      | _ -> loop (i - 1)
      end
  in
  loop (Array.length keys - 1)

and decode_change_optional_duration json =
  let keys = Js.Dict.keys json in
  let rec loop = function 
    | -1 -> Pbrt_bs.E.malformed_variant "change_optional_duration"
    | i -> 
      begin match Array.unsafe_get keys i with
      | "duration" -> 
        let json = Js.Dict.unsafeGet json "duration" in
        (ChangeTypes.Duration (Pbrt_bs.int32 json "change_optional_duration" "Duration") : ChangeTypes.change_optional_duration)
      
      | _ -> loop (i - 1)
      end
  in
  loop (Array.length keys - 1)

and decode_change_optional_self_merged json =
  let keys = Js.Dict.keys json in
  let rec loop = function 
    | -1 -> Pbrt_bs.E.malformed_variant "change_optional_self_merged"
    | i -> 
      begin match Array.unsafe_get keys i with
      | "self_merged" -> 
        let json = Js.Dict.unsafeGet json "self_merged" in
        (ChangeTypes.Self_merged (Pbrt_bs.bool json "change_optional_self_merged" "Self_merged") : ChangeTypes.change_optional_self_merged)
      
      | _ -> loop (i - 1)
      end
  in
  loop (Array.length keys - 1)

and decode_change_optional_merged_commit_sha json =
  let keys = Js.Dict.keys json in
  let rec loop = function 
    | -1 -> Pbrt_bs.E.malformed_variant "change_optional_merged_commit_sha"
    | i -> 
      begin match Array.unsafe_get keys i with
      | "merged_commitSha" -> 
        let json = Js.Dict.unsafeGet json "merged_commitSha" in
        (ChangeTypes.Merged_commit_sha (Pbrt_bs.string json "change_optional_merged_commit_sha" "Merged_commit_sha") : ChangeTypes.change_optional_merged_commit_sha)
      
      | _ -> loop (i - 1)
      end
  in
  loop (Array.length keys - 1)

and decode_change json =
  let v = default_change_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "id" -> 
      let json = Js.Dict.unsafeGet json "id" in
      v.id <- Pbrt_bs.string json "change" "id"
    | "number" -> 
      let json = Js.Dict.unsafeGet json "number" in
      v.number <- Pbrt_bs.int32 json "change" "number"
    | "change_id" -> 
      let json = Js.Dict.unsafeGet json "change_id" in
      v.change_id <- Pbrt_bs.string json "change" "change_id"
    | "title" -> 
      let json = Js.Dict.unsafeGet json "title" in
      v.title <- Pbrt_bs.string json "change" "title"
    | "text" -> 
      let json = Js.Dict.unsafeGet json "text" in
      v.text <- Pbrt_bs.string json "change" "text"
    | "url" -> 
      let json = Js.Dict.unsafeGet json "url" in
      v.url <- Pbrt_bs.string json "change" "url"
    | "commit_count" -> 
      let json = Js.Dict.unsafeGet json "commit_count" in
      v.commit_count <- Pbrt_bs.int32 json "change" "commit_count"
    | "additions" -> 
      let json = Js.Dict.unsafeGet json "additions" in
      v.additions <- Pbrt_bs.int32 json "change" "additions"
    | "deletions" -> 
      let json = Js.Dict.unsafeGet json "deletions" in
      v.deletions <- Pbrt_bs.int32 json "change" "deletions"
    | "changed_files_count" -> 
      let json = Js.Dict.unsafeGet json "changed_files_count" in
      v.changed_files_count <- Pbrt_bs.int32 json "change" "changed_files_count"
    | "changed_files" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "changed_files" in 
        Pbrt_bs.array_ a "change" "changed_files"
      in
      v.changed_files <- Array.map (fun json -> 
        (decode_changed_file (Pbrt_bs.object_ json "change" "changed_files"))
      ) a |> Array.to_list;
    end
    | "commits" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "commits" in 
        Pbrt_bs.array_ a "change" "commits"
      in
      v.commits <- Array.map (fun json -> 
        (decode_commit (Pbrt_bs.object_ json "change" "commits"))
      ) a |> Array.to_list;
    end
    | "repository_prefix" -> 
      let json = Js.Dict.unsafeGet json "repository_prefix" in
      v.repository_prefix <- Pbrt_bs.string json "change" "repository_prefix"
    | "repository_fullname" -> 
      let json = Js.Dict.unsafeGet json "repository_fullname" in
      v.repository_fullname <- Pbrt_bs.string json "change" "repository_fullname"
    | "repository_shortname" -> 
      let json = Js.Dict.unsafeGet json "repository_shortname" in
      v.repository_shortname <- Pbrt_bs.string json "change" "repository_shortname"
    | "author" -> 
      let json = Js.Dict.unsafeGet json "author" in
      v.author <- Some ((decode_ident (Pbrt_bs.object_ json "change" "author")))
    | "merged_by" -> 
      let json = Js.Dict.unsafeGet json "merged_by" in
      v.optional_merged_by <- Merged_by ((decode_ident (Pbrt_bs.object_ json "change" "optional_merged_by")))
    | "branch" -> 
      let json = Js.Dict.unsafeGet json "branch" in
      v.branch <- Pbrt_bs.string json "change" "branch"
    | "target_branch" -> 
      let json = Js.Dict.unsafeGet json "target_branch" in
      v.target_branch <- Pbrt_bs.string json "change" "target_branch"
    | "created_at" -> 
      let json = Js.Dict.unsafeGet json "created_at" in
      v.created_at <- Some ((TimestampBs.decode_timestamp (Pbrt_bs.string json "change" "created_at")))
    | "merged_at" -> 
      let json = Js.Dict.unsafeGet json "merged_at" in
      v.optional_merged_at <- Merged_at ((TimestampBs.decode_timestamp (Pbrt_bs.string json "change" "optional_merged_at")))
    | "updated_at" -> 
      let json = Js.Dict.unsafeGet json "updated_at" in
      v.updated_at <- Some ((TimestampBs.decode_timestamp (Pbrt_bs.string json "change" "updated_at")))
    | "closed_at" -> 
      let json = Js.Dict.unsafeGet json "closed_at" in
      v.optional_closed_at <- Closed_at ((TimestampBs.decode_timestamp (Pbrt_bs.string json "change" "optional_closed_at")))
    | "state" -> 
      let json = Js.Dict.unsafeGet json "state" in
      v.state <- (decode_change_change_state json)
    | "duration" -> 
      let json = Js.Dict.unsafeGet json "duration" in
      v.optional_duration <- Duration (Pbrt_bs.int32 json "change" "optional_duration")
    | "mergeable" -> 
      let json = Js.Dict.unsafeGet json "mergeable" in
      v.mergeable <- Pbrt_bs.string json "change" "mergeable"
    | "labels" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "labels" in 
        Pbrt_bs.array_ a "change" "labels"
      in
      v.labels <- Array.map (fun json -> 
        Pbrt_bs.string json "change" "labels"
      ) a |> Array.to_list;
    end
    | "assignees" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "assignees" in 
        Pbrt_bs.array_ a "change" "assignees"
      in
      v.assignees <- Array.map (fun json -> 
        (decode_ident (Pbrt_bs.object_ json "change" "assignees"))
      ) a |> Array.to_list;
    end
    | "approvals" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "approvals" in 
        Pbrt_bs.array_ a "change" "approvals"
      in
      v.approvals <- Array.map (fun json -> 
        Pbrt_bs.string json "change" "approvals"
      ) a |> Array.to_list;
    end
    | "draft" -> 
      let json = Js.Dict.unsafeGet json "draft" in
      v.draft <- Pbrt_bs.bool json "change" "draft"
    | "self_merged" -> 
      let json = Js.Dict.unsafeGet json "self_merged" in
      v.optional_self_merged <- Self_merged (Pbrt_bs.bool json "change" "optional_self_merged")
    | "merged_commitSha" -> 
      let json = Js.Dict.unsafeGet json "merged_commitSha" in
      v.optional_merged_commit_sha <- Merged_commit_sha (Pbrt_bs.string json "change" "optional_merged_commit_sha")
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    ChangeTypes.id = v.id;
    ChangeTypes.number = v.number;
    ChangeTypes.change_id = v.change_id;
    ChangeTypes.title = v.title;
    ChangeTypes.text = v.text;
    ChangeTypes.url = v.url;
    ChangeTypes.commit_count = v.commit_count;
    ChangeTypes.additions = v.additions;
    ChangeTypes.deletions = v.deletions;
    ChangeTypes.changed_files_count = v.changed_files_count;
    ChangeTypes.changed_files = v.changed_files;
    ChangeTypes.commits = v.commits;
    ChangeTypes.repository_prefix = v.repository_prefix;
    ChangeTypes.repository_fullname = v.repository_fullname;
    ChangeTypes.repository_shortname = v.repository_shortname;
    ChangeTypes.author = v.author;
    ChangeTypes.optional_merged_by = v.optional_merged_by;
    ChangeTypes.branch = v.branch;
    ChangeTypes.target_branch = v.target_branch;
    ChangeTypes.created_at = v.created_at;
    ChangeTypes.optional_merged_at = v.optional_merged_at;
    ChangeTypes.updated_at = v.updated_at;
    ChangeTypes.optional_closed_at = v.optional_closed_at;
    ChangeTypes.state = v.state;
    ChangeTypes.optional_duration = v.optional_duration;
    ChangeTypes.mergeable = v.mergeable;
    ChangeTypes.labels = v.labels;
    ChangeTypes.assignees = v.assignees;
    ChangeTypes.approvals = v.approvals;
    ChangeTypes.draft = v.draft;
    ChangeTypes.optional_self_merged = v.optional_self_merged;
    ChangeTypes.optional_merged_commit_sha = v.optional_merged_commit_sha;
  } : ChangeTypes.change)

let rec decode_change_reviewed_event json =
  let v = default_change_reviewed_event_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "approvals" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "approvals" in 
        Pbrt_bs.array_ a "change_reviewed_event" "approvals"
      in
      v.approvals <- Array.map (fun json -> 
        Pbrt_bs.string json "change_reviewed_event" "approvals"
      ) a |> Array.to_list;
    end
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    ChangeTypes.approvals = v.approvals;
  } : ChangeTypes.change_reviewed_event)

let rec decode_change_event_type json =
  let keys = Js.Dict.keys json in
  let rec loop = function 
    | -1 -> Pbrt_bs.E.malformed_variant "change_event_type"
    | i -> 
      begin match Array.unsafe_get keys i with
      | "change_created" -> (ChangeTypes.Change_created : ChangeTypes.change_event_type)
      | "change_commented" -> (ChangeTypes.Change_commented : ChangeTypes.change_event_type)
      | "change_abandoned" -> (ChangeTypes.Change_abandoned : ChangeTypes.change_event_type)
      | "change_reviewed" -> 
        let json = Js.Dict.unsafeGet json "change_reviewed" in
        (ChangeTypes.Change_reviewed ((decode_change_reviewed_event (Pbrt_bs.object_ json "change_event_type" "Change_reviewed"))) : ChangeTypes.change_event_type)
      | "change_commitForcePushed" -> (ChangeTypes.Change_commit_force_pushed : ChangeTypes.change_event_type)
      | "change_commitPushed" -> (ChangeTypes.Change_commit_pushed : ChangeTypes.change_event_type)
      | "change_merged" -> (ChangeTypes.Change_merged : ChangeTypes.change_event_type)
      
      | _ -> loop (i - 1)
      end
  in
  loop (Array.length keys - 1)

and decode_change_event_optional_duration json =
  let keys = Js.Dict.keys json in
  let rec loop = function 
    | -1 -> Pbrt_bs.E.malformed_variant "change_event_optional_duration"
    | i -> 
      begin match Array.unsafe_get keys i with
      | "duration" -> 
        let json = Js.Dict.unsafeGet json "duration" in
        (ChangeTypes.Duration (Pbrt_bs.int32 json "change_event_optional_duration" "Duration") : ChangeTypes.change_event_optional_duration)
      
      | _ -> loop (i - 1)
      end
  in
  loop (Array.length keys - 1)

and decode_change_event_optional_merged_commit_sha json =
  let keys = Js.Dict.keys json in
  let rec loop = function 
    | -1 -> Pbrt_bs.E.malformed_variant "change_event_optional_merged_commit_sha"
    | i -> 
      begin match Array.unsafe_get keys i with
      | "merged_commitSha" -> 
        let json = Js.Dict.unsafeGet json "merged_commitSha" in
        (ChangeTypes.Merged_commit_sha (Pbrt_bs.string json "change_event_optional_merged_commit_sha" "Merged_commit_sha") : ChangeTypes.change_event_optional_merged_commit_sha)
      
      | _ -> loop (i - 1)
      end
  in
  loop (Array.length keys - 1)

and decode_change_event json =
  let v = default_change_event_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "id" -> 
      let json = Js.Dict.unsafeGet json "id" in
      v.id <- Pbrt_bs.string json "change_event" "id"
    | "created_at" -> 
      let json = Js.Dict.unsafeGet json "created_at" in
      v.created_at <- Some ((TimestampBs.decode_timestamp (Pbrt_bs.string json "change_event" "created_at")))
    | "author" -> 
      let json = Js.Dict.unsafeGet json "author" in
      v.author <- Some ((decode_ident (Pbrt_bs.object_ json "change_event" "author")))
    | "repository_prefix" -> 
      let json = Js.Dict.unsafeGet json "repository_prefix" in
      v.repository_prefix <- Pbrt_bs.string json "change_event" "repository_prefix"
    | "repository_fullname" -> 
      let json = Js.Dict.unsafeGet json "repository_fullname" in
      v.repository_fullname <- Pbrt_bs.string json "change_event" "repository_fullname"
    | "repository_shortname" -> 
      let json = Js.Dict.unsafeGet json "repository_shortname" in
      v.repository_shortname <- Pbrt_bs.string json "change_event" "repository_shortname"
    | "branch" -> 
      let json = Js.Dict.unsafeGet json "branch" in
      v.branch <- Pbrt_bs.string json "change_event" "branch"
    | "target_branch" -> 
      let json = Js.Dict.unsafeGet json "target_branch" in
      v.target_branch <- Pbrt_bs.string json "change_event" "target_branch"
    | "number" -> 
      let json = Js.Dict.unsafeGet json "number" in
      v.number <- Pbrt_bs.int32 json "change_event" "number"
    | "change_id" -> 
      let json = Js.Dict.unsafeGet json "change_id" in
      v.change_id <- Pbrt_bs.string json "change_event" "change_id"
    | "url" -> 
      let json = Js.Dict.unsafeGet json "url" in
      v.url <- Pbrt_bs.string json "change_event" "url"
    | "on_author" -> 
      let json = Js.Dict.unsafeGet json "on_author" in
      v.on_author <- Some ((decode_ident (Pbrt_bs.object_ json "change_event" "on_author")))
    | "on_created_at" -> 
      let json = Js.Dict.unsafeGet json "on_created_at" in
      v.on_created_at <- Some ((TimestampBs.decode_timestamp (Pbrt_bs.string json "change_event" "on_created_at")))
    | "changed_files" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "changed_files" in 
        Pbrt_bs.array_ a "change_event" "changed_files"
      in
      v.changed_files <- Array.map (fun json -> 
        (decode_changed_file_path (Pbrt_bs.object_ json "change_event" "changed_files"))
      ) a |> Array.to_list;
    end
    | "change_created" -> v.type_ <- Change_created
    | "change_commented" -> v.type_ <- Change_commented
    | "change_abandoned" -> v.type_ <- Change_abandoned
    | "change_reviewed" -> 
      let json = Js.Dict.unsafeGet json "change_reviewed" in
      v.type_ <- Change_reviewed ((decode_change_reviewed_event (Pbrt_bs.object_ json "change_event" "type_")))
    | "change_commitForcePushed" -> v.type_ <- Change_commit_force_pushed
    | "change_commitPushed" -> v.type_ <- Change_commit_pushed
    | "change_merged" -> v.type_ <- Change_merged
    | "labels" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "labels" in 
        Pbrt_bs.array_ a "change_event" "labels"
      in
      v.labels <- Array.map (fun json -> 
        Pbrt_bs.string json "change_event" "labels"
      ) a |> Array.to_list;
    end
    | "duration" -> 
      let json = Js.Dict.unsafeGet json "duration" in
      v.optional_duration <- Duration (Pbrt_bs.int32 json "change_event" "optional_duration")
    | "draft" -> 
      let json = Js.Dict.unsafeGet json "draft" in
      v.draft <- Pbrt_bs.bool json "change_event" "draft"
    | "merged_commitSha" -> 
      let json = Js.Dict.unsafeGet json "merged_commitSha" in
      v.optional_merged_commit_sha <- Merged_commit_sha (Pbrt_bs.string json "change_event" "optional_merged_commit_sha")
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    ChangeTypes.id = v.id;
    ChangeTypes.created_at = v.created_at;
    ChangeTypes.author = v.author;
    ChangeTypes.repository_prefix = v.repository_prefix;
    ChangeTypes.repository_fullname = v.repository_fullname;
    ChangeTypes.repository_shortname = v.repository_shortname;
    ChangeTypes.branch = v.branch;
    ChangeTypes.target_branch = v.target_branch;
    ChangeTypes.number = v.number;
    ChangeTypes.change_id = v.change_id;
    ChangeTypes.url = v.url;
    ChangeTypes.on_author = v.on_author;
    ChangeTypes.on_created_at = v.on_created_at;
    ChangeTypes.changed_files = v.changed_files;
    ChangeTypes.type_ = v.type_;
    ChangeTypes.labels = v.labels;
    ChangeTypes.optional_duration = v.optional_duration;
    ChangeTypes.draft = v.draft;
    ChangeTypes.optional_merged_commit_sha = v.optional_merged_commit_sha;
  } : ChangeTypes.change_event)

let rec encode_ident (v:ChangeTypes.ident) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "uid" (Js.Json.string v.ChangeTypes.uid);
  Js.Dict.set json "muid" (Js.Json.string v.ChangeTypes.muid);
  begin match v.ChangeTypes.groups with
  | [] -> ()
  | __x__ -> (* groups *)
    let a = __x__ |> Array.of_list |> Array.map Js.Json.string in
    Js.Dict.set json "groups" (Js.Json.array a);
  end;
  json

let rec encode_changed_file (v:ChangeTypes.changed_file) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "additions" (Js.Json.number (Int32.to_float v.ChangeTypes.additions));
  Js.Dict.set json "deletions" (Js.Json.number (Int32.to_float v.ChangeTypes.deletions));
  Js.Dict.set json "path" (Js.Json.string v.ChangeTypes.path);
  json

let rec encode_changed_file_path (v:ChangeTypes.changed_file_path) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "path" (Js.Json.string v.ChangeTypes.path);
  json

let rec encode_commit (v:ChangeTypes.commit) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "sha" (Js.Json.string v.ChangeTypes.sha);
  begin match v.ChangeTypes.author with
  | None -> ()
  | Some v ->
    begin (* author field *)
      let json' = encode_ident v in
      Js.Dict.set json "author" (Js.Json.object_ json');
    end;
  end;
  begin match v.ChangeTypes.committer with
  | None -> ()
  | Some v ->
    begin (* committer field *)
      let json' = encode_ident v in
      Js.Dict.set json "committer" (Js.Json.object_ json');
    end;
  end;
  begin match v.ChangeTypes.authored_at with
  | None -> ()
  | Some v ->
    begin (* authored_at field *)
      let json' = TimestampBs.encode_timestamp v in
      Js.Dict.set json "authored_at" (Js.Json.string json');
    end;
  end;
  begin match v.ChangeTypes.committed_at with
  | None -> ()
  | Some v ->
    begin (* committed_at field *)
      let json' = TimestampBs.encode_timestamp v in
      Js.Dict.set json "committed_at" (Js.Json.string json');
    end;
  end;
  Js.Dict.set json "additions" (Js.Json.number (Int32.to_float v.ChangeTypes.additions));
  Js.Dict.set json "deletions" (Js.Json.number (Int32.to_float v.ChangeTypes.deletions));
  Js.Dict.set json "title" (Js.Json.string v.ChangeTypes.title);
  json

let rec encode_change_change_state (v:ChangeTypes.change_change_state) : string = 
  match v with
  | ChangeTypes.Open -> "Open"
  | ChangeTypes.Merged -> "Merged"
  | ChangeTypes.Closed -> "Closed"

let rec encode_change_optional_merged_by (v:ChangeTypes.change_optional_merged_by) = 
  let json = Js.Dict.empty () in
  begin match v with
  | ChangeTypes.Merged_by v ->
    begin (* mergedBy field *)
      let json' = encode_ident v in
      Js.Dict.set json "merged_by" (Js.Json.object_ json');
    end;
  end;
  json

and encode_change_optional_merged_at (v:ChangeTypes.change_optional_merged_at) = 
  let json = Js.Dict.empty () in
  begin match v with
  | ChangeTypes.Merged_at v ->
    begin (* merged_at field *)
      let json' = TimestampBs.encode_timestamp v in
      Js.Dict.set json "merged_at" (Js.Json.string json');
    end;
  end;
  json

and encode_change_optional_closed_at (v:ChangeTypes.change_optional_closed_at) = 
  let json = Js.Dict.empty () in
  begin match v with
  | ChangeTypes.Closed_at v ->
    begin (* closed_at field *)
      let json' = TimestampBs.encode_timestamp v in
      Js.Dict.set json "closed_at" (Js.Json.string json');
    end;
  end;
  json

and encode_change_optional_duration (v:ChangeTypes.change_optional_duration) = 
  let json = Js.Dict.empty () in
  begin match v with
  | ChangeTypes.Duration v ->
    Js.Dict.set json "duration" (Js.Json.number (Int32.to_float v));
  end;
  json

and encode_change_optional_self_merged (v:ChangeTypes.change_optional_self_merged) = 
  let json = Js.Dict.empty () in
  begin match v with
  | ChangeTypes.Self_merged v ->
    Js.Dict.set json "self_merged" (Js.Json.boolean v);
  end;
  json

and encode_change_optional_merged_commit_sha (v:ChangeTypes.change_optional_merged_commit_sha) = 
  let json = Js.Dict.empty () in
  begin match v with
  | ChangeTypes.Merged_commit_sha v ->
    Js.Dict.set json "merged_commitSha" (Js.Json.string v);
  end;
  json

and encode_change (v:ChangeTypes.change) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "id" (Js.Json.string v.ChangeTypes.id);
  Js.Dict.set json "number" (Js.Json.number (Int32.to_float v.ChangeTypes.number));
  Js.Dict.set json "change_id" (Js.Json.string v.ChangeTypes.change_id);
  Js.Dict.set json "title" (Js.Json.string v.ChangeTypes.title);
  Js.Dict.set json "text" (Js.Json.string v.ChangeTypes.text);
  Js.Dict.set json "url" (Js.Json.string v.ChangeTypes.url);
  Js.Dict.set json "commit_count" (Js.Json.number (Int32.to_float v.ChangeTypes.commit_count));
  Js.Dict.set json "additions" (Js.Json.number (Int32.to_float v.ChangeTypes.additions));
  Js.Dict.set json "deletions" (Js.Json.number (Int32.to_float v.ChangeTypes.deletions));
  Js.Dict.set json "changed_files_count" (Js.Json.number (Int32.to_float v.ChangeTypes.changed_files_count));
  begin match v.ChangeTypes.changed_files with
  | [] -> ()
  | __x__ -> (* changedFiles *)
    let (changed_files':Js.Json.t) =
      __x__
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_changed_file |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "changed_files" changed_files'
  end;
  begin match v.ChangeTypes.commits with
  | [] -> ()
  | __x__ -> (* commits *)
    let (commits':Js.Json.t) =
      __x__
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_commit |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "commits" commits'
  end;
  Js.Dict.set json "repository_prefix" (Js.Json.string v.ChangeTypes.repository_prefix);
  Js.Dict.set json "repository_fullname" (Js.Json.string v.ChangeTypes.repository_fullname);
  Js.Dict.set json "repository_shortname" (Js.Json.string v.ChangeTypes.repository_shortname);
  begin match v.ChangeTypes.author with
  | None -> ()
  | Some v ->
    begin (* author field *)
      let json' = encode_ident v in
      Js.Dict.set json "author" (Js.Json.object_ json');
    end;
  end;
  begin match v.ChangeTypes.optional_merged_by with
    | Merged_by v ->
      begin (* mergedBy field *)
        let json' = encode_ident v in
        Js.Dict.set json "merged_by" (Js.Json.object_ json');
      end;
  end; (* match v.optional_merged_by *)
  Js.Dict.set json "branch" (Js.Json.string v.ChangeTypes.branch);
  Js.Dict.set json "target_branch" (Js.Json.string v.ChangeTypes.target_branch);
  begin match v.ChangeTypes.created_at with
  | None -> ()
  | Some v ->
    begin (* created_at field *)
      let json' = TimestampBs.encode_timestamp v in
      Js.Dict.set json "created_at" (Js.Json.string json');
    end;
  end;
  begin match v.ChangeTypes.optional_merged_at with
    | Merged_at v ->
      begin (* merged_at field *)
        let json' = TimestampBs.encode_timestamp v in
        Js.Dict.set json "merged_at" (Js.Json.string json');
      end;
  end; (* match v.optional_merged_at *)
  begin match v.ChangeTypes.updated_at with
  | None -> ()
  | Some v ->
    begin (* updated_at field *)
      let json' = TimestampBs.encode_timestamp v in
      Js.Dict.set json "updated_at" (Js.Json.string json');
    end;
  end;
  begin match v.ChangeTypes.optional_closed_at with
    | Closed_at v ->
      begin (* closed_at field *)
        let json' = TimestampBs.encode_timestamp v in
        Js.Dict.set json "closed_at" (Js.Json.string json');
      end;
  end; (* match v.optional_closed_at *)
  Js.Dict.set json "state" (Js.Json.string (encode_change_change_state v.ChangeTypes.state));
  begin match v.ChangeTypes.optional_duration with
    | Duration v ->
      Js.Dict.set json "duration" (Js.Json.number (Int32.to_float v));
  end; (* match v.optional_duration *)
  Js.Dict.set json "mergeable" (Js.Json.string v.ChangeTypes.mergeable);
  begin match v.ChangeTypes.labels with
  | [] -> ()
  | __x__ -> (* labels *)
    let a = __x__ |> Array.of_list |> Array.map Js.Json.string in
    Js.Dict.set json "labels" (Js.Json.array a);
  end;
  begin match v.ChangeTypes.assignees with
  | [] -> ()
  | __x__ -> (* assignees *)
    let (assignees':Js.Json.t) =
      __x__
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_ident |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "assignees" assignees'
  end;
  begin match v.ChangeTypes.approvals with
  | [] -> ()
  | __x__ -> (* approvals *)
    let a = __x__ |> Array.of_list |> Array.map Js.Json.string in
    Js.Dict.set json "approvals" (Js.Json.array a);
  end;
  Js.Dict.set json "draft" (Js.Json.boolean v.ChangeTypes.draft);
  begin match v.ChangeTypes.optional_self_merged with
    | Self_merged v ->
      Js.Dict.set json "self_merged" (Js.Json.boolean v);
  end; (* match v.optional_self_merged *)
  begin match v.ChangeTypes.optional_merged_commit_sha with
    | Merged_commit_sha v ->
      Js.Dict.set json "merged_commitSha" (Js.Json.string v);
  end; (* match v.optional_merged_commit_sha *)
  json

let rec encode_change_reviewed_event (v:ChangeTypes.change_reviewed_event) = 
  let json = Js.Dict.empty () in
  begin match v.ChangeTypes.approvals with
  | [] -> ()
  | __x__ -> (* approvals *)
    let a = __x__ |> Array.of_list |> Array.map Js.Json.string in
    Js.Dict.set json "approvals" (Js.Json.array a);
  end;
  json

let rec encode_change_event_type (v:ChangeTypes.change_event_type) = 
  let json = Js.Dict.empty () in
  begin match v with
  | ChangeTypes.Change_created ->
    Js.Dict.set json "change_created" Js.Json.null
  | ChangeTypes.Change_commented ->
    Js.Dict.set json "change_commented" Js.Json.null
  | ChangeTypes.Change_abandoned ->
    Js.Dict.set json "change_abandoned" Js.Json.null
  | ChangeTypes.Change_reviewed v ->
    begin (* changeReviewed field *)
      let json' = encode_change_reviewed_event v in
      Js.Dict.set json "change_reviewed" (Js.Json.object_ json');
    end;
  | ChangeTypes.Change_commit_force_pushed ->
    Js.Dict.set json "change_commitForcePushed" Js.Json.null
  | ChangeTypes.Change_commit_pushed ->
    Js.Dict.set json "change_commitPushed" Js.Json.null
  | ChangeTypes.Change_merged ->
    Js.Dict.set json "change_merged" Js.Json.null
  end;
  json

and encode_change_event_optional_duration (v:ChangeTypes.change_event_optional_duration) = 
  let json = Js.Dict.empty () in
  begin match v with
  | ChangeTypes.Duration v ->
    Js.Dict.set json "duration" (Js.Json.number (Int32.to_float v));
  end;
  json

and encode_change_event_optional_merged_commit_sha (v:ChangeTypes.change_event_optional_merged_commit_sha) = 
  let json = Js.Dict.empty () in
  begin match v with
  | ChangeTypes.Merged_commit_sha v ->
    Js.Dict.set json "merged_commitSha" (Js.Json.string v);
  end;
  json

and encode_change_event (v:ChangeTypes.change_event) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "id" (Js.Json.string v.ChangeTypes.id);
  begin match v.ChangeTypes.created_at with
  | None -> ()
  | Some v ->
    begin (* created_at field *)
      let json' = TimestampBs.encode_timestamp v in
      Js.Dict.set json "created_at" (Js.Json.string json');
    end;
  end;
  begin match v.ChangeTypes.author with
  | None -> ()
  | Some v ->
    begin (* author field *)
      let json' = encode_ident v in
      Js.Dict.set json "author" (Js.Json.object_ json');
    end;
  end;
  Js.Dict.set json "repository_prefix" (Js.Json.string v.ChangeTypes.repository_prefix);
  Js.Dict.set json "repository_fullname" (Js.Json.string v.ChangeTypes.repository_fullname);
  Js.Dict.set json "repository_shortname" (Js.Json.string v.ChangeTypes.repository_shortname);
  Js.Dict.set json "branch" (Js.Json.string v.ChangeTypes.branch);
  Js.Dict.set json "target_branch" (Js.Json.string v.ChangeTypes.target_branch);
  Js.Dict.set json "number" (Js.Json.number (Int32.to_float v.ChangeTypes.number));
  Js.Dict.set json "change_id" (Js.Json.string v.ChangeTypes.change_id);
  Js.Dict.set json "url" (Js.Json.string v.ChangeTypes.url);
  begin match v.ChangeTypes.on_author with
  | None -> ()
  | Some v ->
    begin (* onAuthor field *)
      let json' = encode_ident v in
      Js.Dict.set json "on_author" (Js.Json.object_ json');
    end;
  end;
  begin match v.ChangeTypes.on_created_at with
  | None -> ()
  | Some v ->
    begin (* onCreated_at field *)
      let json' = TimestampBs.encode_timestamp v in
      Js.Dict.set json "on_created_at" (Js.Json.string json');
    end;
  end;
  begin match v.ChangeTypes.changed_files with
  | [] -> ()
  | __x__ -> (* changedFiles *)
    let (changed_files':Js.Json.t) =
      __x__
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_changed_file_path |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "changed_files" changed_files'
  end;
  begin match v.ChangeTypes.type_ with
    | Change_created ->
      Js.Dict.set json "change_created" Js.Json.null
    | Change_commented ->
      Js.Dict.set json "change_commented" Js.Json.null
    | Change_abandoned ->
      Js.Dict.set json "change_abandoned" Js.Json.null
    | Change_reviewed v ->
      begin (* changeReviewed field *)
        let json' = encode_change_reviewed_event v in
        Js.Dict.set json "change_reviewed" (Js.Json.object_ json');
      end;
    | Change_commit_force_pushed ->
      Js.Dict.set json "change_commitForcePushed" Js.Json.null
    | Change_commit_pushed ->
      Js.Dict.set json "change_commitPushed" Js.Json.null
    | Change_merged ->
      Js.Dict.set json "change_merged" Js.Json.null
  end; (* match v.type_ *)
  begin match v.ChangeTypes.labels with
  | [] -> ()
  | __x__ -> (* labels *)
    let a = __x__ |> Array.of_list |> Array.map Js.Json.string in
    Js.Dict.set json "labels" (Js.Json.array a);
  end;
  begin match v.ChangeTypes.optional_duration with
    | Duration v ->
      Js.Dict.set json "duration" (Js.Json.number (Int32.to_float v));
  end; (* match v.optional_duration *)
  Js.Dict.set json "draft" (Js.Json.boolean v.ChangeTypes.draft);
  begin match v.ChangeTypes.optional_merged_commit_sha with
    | Merged_commit_sha v ->
      Js.Dict.set json "merged_commitSha" (Js.Json.string v);
  end; (* match v.optional_merged_commit_sha *)
  json
