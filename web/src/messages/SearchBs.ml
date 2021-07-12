[@@@ocaml.warning "-27-30-39"]

type search_suggestions_request_mutable = {
  mutable index : string;
}

let default_search_suggestions_request_mutable () : search_suggestions_request_mutable = {
  index = "";
}

type search_suggestions_response_mutable = {
  mutable task_types : string list;
  mutable authors : string list;
  mutable approvals : string list;
  mutable priorities : string list;
  mutable severities : string list;
}

let default_search_suggestions_response_mutable () : search_suggestions_response_mutable = {
  task_types = [];
  authors = [];
  approvals = [];
  priorities = [];
  severities = [];
}

type fields_request_mutable = {
  mutable version : string;
}

let default_fields_request_mutable () : fields_request_mutable = {
  version = "";
}

type field_mutable = {
  mutable name : string;
  mutable description : string;
  mutable type_ : SearchTypes.field_type;
}

let default_field_mutable () : field_mutable = {
  name = "";
  description = "";
  type_ = SearchTypes.default_field_type ();
}

type fields_response_mutable = {
  mutable fields : SearchTypes.field list;
}

let default_fields_response_mutable () : fields_response_mutable = {
  fields = [];
}

type query_error_mutable = {
  mutable message : string;
  mutable position : int32;
}

let default_query_error_mutable () : query_error_mutable = {
  message = "";
  position = 0l;
}

type order_mutable = {
  mutable field : string;
  mutable direction : SearchTypes.order_direction;
}

let default_order_mutable () : order_mutable = {
  field = "";
  direction = SearchTypes.default_order_direction ();
}

type query_request_mutable = {
  mutable index : string;
  mutable username : string;
  mutable query : string;
  mutable query_type : SearchTypes.query_request_query_type;
  mutable order : SearchTypes.order option;
  mutable limit : int32;
}

let default_query_request_mutable () : query_request_mutable = {
  index = "";
  username = "";
  query = "";
  query_type = SearchTypes.default_query_request_query_type ();
  order = None;
  limit = 0l;
}

type file_mutable = {
  mutable additions : int32;
  mutable deletions : int32;
  mutable path : string;
}

let default_file_mutable () : file_mutable = {
  additions = 0l;
  deletions = 0l;
  path = "";
}

type commit_mutable = {
  mutable sha : string;
  mutable title : string;
  mutable author : string;
  mutable authored_at : TimestampTypes.timestamp option;
  mutable committer : string;
  mutable committed_at : TimestampTypes.timestamp option;
  mutable additions : int32;
  mutable deletions : int32;
}

let default_commit_mutable () : commit_mutable = {
  sha = "";
  title = "";
  author = "";
  authored_at = None;
  committer = "";
  committed_at = None;
  additions = 0l;
  deletions = 0l;
}

type change_mutable = {
  mutable change_id : string;
  mutable author : string;
  mutable title : string;
  mutable url : string;
  mutable repository_fullname : string;
  mutable state : string;
  mutable branch : string;
  mutable target_branch : string;
  mutable created_at : TimestampTypes.timestamp option;
  mutable updated_at : TimestampTypes.timestamp option;
  mutable merged_at : TimestampTypes.timestamp option;
  mutable merged_by_m : SearchTypes.change_merged_by_m;
  mutable text : string;
  mutable additions : int32;
  mutable deletions : int32;
  mutable approval : string list;
  mutable assignees : string list;
  mutable labels : string list;
  mutable draft : bool;
  mutable mergeable : bool;
  mutable changed_files : SearchTypes.file list;
  mutable changed_files_count : int32;
  mutable commits : SearchTypes.commit list;
  mutable commits_count : int32;
  mutable task_data : TaskDataTypes.task_data list;
}

let default_change_mutable () : change_mutable = {
  change_id = "";
  author = "";
  title = "";
  url = "";
  repository_fullname = "";
  state = "";
  branch = "";
  target_branch = "";
  created_at = None;
  updated_at = None;
  merged_at = None;
  merged_by_m = SearchTypes.Merged_by ("");
  text = "";
  additions = 0l;
  deletions = 0l;
  approval = [];
  assignees = [];
  labels = [];
  draft = false;
  mergeable = false;
  changed_files = [];
  changed_files_count = 0l;
  commits = [];
  commits_count = 0l;
  task_data = [];
}

type changes_mutable = {
  mutable changes : SearchTypes.change list;
}

let default_changes_mutable () : changes_mutable = {
  changes = [];
}

type repo_summary_mutable = {
  mutable fullname : string;
  mutable total_changes : int32;
  mutable abandoned_changes : int32;
  mutable merged_changes : int32;
  mutable open_changes : int32;
}

let default_repo_summary_mutable () : repo_summary_mutable = {
  fullname = "";
  total_changes = 0l;
  abandoned_changes = 0l;
  merged_changes = 0l;
  open_changes = 0l;
}

type repos_summary_mutable = {
  mutable repository_summary : SearchTypes.repo_summary list;
}

let default_repos_summary_mutable () : repos_summary_mutable = {
  repository_summary = [];
}

type changes_histos_event_mutable = {
  mutable doc_count : int32;
  mutable key : int64;
  mutable key_as_string : string;
}

let default_changes_histos_event_mutable () : changes_histos_event_mutable = {
  doc_count = 0l;
  key = 0L;
  key_as_string = "";
}

type changes_histos_mutable = {
  mutable change_abandoned_event : SearchTypes.changes_histos_event list;
  mutable change_commit_force_pushed_event : SearchTypes.changes_histos_event list;
  mutable change_commit_pushed_event : SearchTypes.changes_histos_event list;
  mutable change_created_event : SearchTypes.changes_histos_event list;
  mutable change_merged_event : SearchTypes.changes_histos_event list;
}

let default_changes_histos_mutable () : changes_histos_mutable = {
  change_abandoned_event = [];
  change_commit_force_pushed_event = [];
  change_commit_pushed_event = [];
  change_created_event = [];
  change_merged_event = [];
}

type changes_lifecycle_event_mutable = {
  mutable authors_count : int32;
  mutable events_count : int32;
}

let default_changes_lifecycle_event_mutable () : changes_lifecycle_event_mutable = {
  authors_count = 0l;
  events_count = 0l;
}

type changes_lifecycle_ratios_mutable = {
  mutable abandoned : float;
  mutable iterations : float;
  mutable merged : float;
  mutable self_merged : float;
}

let default_changes_lifecycle_ratios_mutable () : changes_lifecycle_ratios_mutable = {
  abandoned = 0.;
  iterations = 0.;
  merged = 0.;
  self_merged = 0.;
}

type changes_lifecycle_mutable = {
  mutable change_commit_force_pushed_event : SearchTypes.changes_lifecycle_event option;
  mutable change_commit_pushed_event : SearchTypes.changes_lifecycle_event option;
  mutable change_created_event : SearchTypes.changes_lifecycle_event option;
  mutable abandoned : int32;
  mutable commits : float;
  mutable duration : float;
  mutable duration_variability : float;
  mutable histos : SearchTypes.changes_histos option;
  mutable merged : int32;
  mutable opened : int32;
  mutable ratios : SearchTypes.changes_lifecycle_ratios option;
  mutable self_merged : int32;
  mutable tests : float;
}

let default_changes_lifecycle_mutable () : changes_lifecycle_mutable = {
  change_commit_force_pushed_event = None;
  change_commit_pushed_event = None;
  change_created_event = None;
  abandoned = 0l;
  commits = 0.;
  duration = 0.;
  duration_variability = 0.;
  histos = None;
  merged = 0l;
  opened = 0l;
  ratios = None;
  self_merged = 0l;
  tests = 0.;
}


let rec decode_search_suggestions_request json =
  let v = default_search_suggestions_request_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "index" -> 
      let json = Js.Dict.unsafeGet json "index" in
      v.index <- Pbrt_bs.string json "search_suggestions_request" "index"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    SearchTypes.index = v.index;
  } : SearchTypes.search_suggestions_request)

let rec decode_search_suggestions_response json =
  let v = default_search_suggestions_response_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "task_types" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "task_types" in 
        Pbrt_bs.array_ a "search_suggestions_response" "task_types"
      in
      v.task_types <- Array.map (fun json -> 
        Pbrt_bs.string json "search_suggestions_response" "task_types"
      ) a |> Array.to_list;
    end
    | "authors" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "authors" in 
        Pbrt_bs.array_ a "search_suggestions_response" "authors"
      in
      v.authors <- Array.map (fun json -> 
        Pbrt_bs.string json "search_suggestions_response" "authors"
      ) a |> Array.to_list;
    end
    | "approvals" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "approvals" in 
        Pbrt_bs.array_ a "search_suggestions_response" "approvals"
      in
      v.approvals <- Array.map (fun json -> 
        Pbrt_bs.string json "search_suggestions_response" "approvals"
      ) a |> Array.to_list;
    end
    | "priorities" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "priorities" in 
        Pbrt_bs.array_ a "search_suggestions_response" "priorities"
      in
      v.priorities <- Array.map (fun json -> 
        Pbrt_bs.string json "search_suggestions_response" "priorities"
      ) a |> Array.to_list;
    end
    | "severities" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "severities" in 
        Pbrt_bs.array_ a "search_suggestions_response" "severities"
      in
      v.severities <- Array.map (fun json -> 
        Pbrt_bs.string json "search_suggestions_response" "severities"
      ) a |> Array.to_list;
    end
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    SearchTypes.task_types = v.task_types;
    SearchTypes.authors = v.authors;
    SearchTypes.approvals = v.approvals;
    SearchTypes.priorities = v.priorities;
    SearchTypes.severities = v.severities;
  } : SearchTypes.search_suggestions_response)

let rec decode_fields_request json =
  let v = default_fields_request_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "version" -> 
      let json = Js.Dict.unsafeGet json "version" in
      v.version <- Pbrt_bs.string json "fields_request" "version"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    SearchTypes.version = v.version;
  } : SearchTypes.fields_request)

let rec decode_field_type (json:Js.Json.t) =
  match Pbrt_bs.string json "field_type" "value" with
  | "FIELD_DATE" -> (SearchTypes.Field_date : SearchTypes.field_type)
  | "FIELD_NUMBER" -> (SearchTypes.Field_number : SearchTypes.field_type)
  | "FIELD_TEXT" -> (SearchTypes.Field_text : SearchTypes.field_type)
  | "FIELD_BOOL" -> (SearchTypes.Field_bool : SearchTypes.field_type)
  | "FIELD_REGEX" -> (SearchTypes.Field_regex : SearchTypes.field_type)
  | "" -> SearchTypes.Field_date
  | _ -> Pbrt_bs.E.malformed_variant "field_type"

let rec decode_field json =
  let v = default_field_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "name" -> 
      let json = Js.Dict.unsafeGet json "name" in
      v.name <- Pbrt_bs.string json "field" "name"
    | "description" -> 
      let json = Js.Dict.unsafeGet json "description" in
      v.description <- Pbrt_bs.string json "field" "description"
    | "type" -> 
      let json = Js.Dict.unsafeGet json "type" in
      v.type_ <- (decode_field_type json)
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    SearchTypes.name = v.name;
    SearchTypes.description = v.description;
    SearchTypes.type_ = v.type_;
  } : SearchTypes.field)

let rec decode_fields_response json =
  let v = default_fields_response_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "fields" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "fields" in 
        Pbrt_bs.array_ a "fields_response" "fields"
      in
      v.fields <- Array.map (fun json -> 
        (decode_field (Pbrt_bs.object_ json "fields_response" "fields"))
      ) a |> Array.to_list;
    end
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    SearchTypes.fields = v.fields;
  } : SearchTypes.fields_response)

let rec decode_query_error json =
  let v = default_query_error_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "message" -> 
      let json = Js.Dict.unsafeGet json "message" in
      v.message <- Pbrt_bs.string json "query_error" "message"
    | "position" -> 
      let json = Js.Dict.unsafeGet json "position" in
      v.position <- Pbrt_bs.int32 json "query_error" "position"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    SearchTypes.message = v.message;
    SearchTypes.position = v.position;
  } : SearchTypes.query_error)

let rec decode_order_direction (json:Js.Json.t) =
  match Pbrt_bs.string json "order_direction" "value" with
  | "ASC" -> (SearchTypes.Asc : SearchTypes.order_direction)
  | "DESC" -> (SearchTypes.Desc : SearchTypes.order_direction)
  | "" -> SearchTypes.Asc
  | _ -> Pbrt_bs.E.malformed_variant "order_direction"

let rec decode_order json =
  let v = default_order_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "field" -> 
      let json = Js.Dict.unsafeGet json "field" in
      v.field <- Pbrt_bs.string json "order" "field"
    | "direction" -> 
      let json = Js.Dict.unsafeGet json "direction" in
      v.direction <- (decode_order_direction json)
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    SearchTypes.field = v.field;
    SearchTypes.direction = v.direction;
  } : SearchTypes.order)

let rec decode_query_request_query_type (json:Js.Json.t) =
  match Pbrt_bs.string json "query_request_query_type" "value" with
  | "QUERY_CHANGE" -> (SearchTypes.Query_change : SearchTypes.query_request_query_type)
  | "QUERY_CHANGE_LIFECYCLE" -> (SearchTypes.Query_change_lifecycle : SearchTypes.query_request_query_type)
  | "QUERY_REPOS_SUMMARY" -> (SearchTypes.Query_repos_summary : SearchTypes.query_request_query_type)
  | "" -> SearchTypes.Query_change
  | _ -> Pbrt_bs.E.malformed_variant "query_request_query_type"

let rec decode_query_request json =
  let v = default_query_request_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "index" -> 
      let json = Js.Dict.unsafeGet json "index" in
      v.index <- Pbrt_bs.string json "query_request" "index"
    | "username" -> 
      let json = Js.Dict.unsafeGet json "username" in
      v.username <- Pbrt_bs.string json "query_request" "username"
    | "query" -> 
      let json = Js.Dict.unsafeGet json "query" in
      v.query <- Pbrt_bs.string json "query_request" "query"
    | "query_type" -> 
      let json = Js.Dict.unsafeGet json "query_type" in
      v.query_type <- (decode_query_request_query_type json)
    | "order" -> 
      let json = Js.Dict.unsafeGet json "order" in
      v.order <- Some ((decode_order (Pbrt_bs.object_ json "query_request" "order")))
    | "limit" -> 
      let json = Js.Dict.unsafeGet json "limit" in
      v.limit <- Pbrt_bs.int32 json "query_request" "limit"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    SearchTypes.index = v.index;
    SearchTypes.username = v.username;
    SearchTypes.query = v.query;
    SearchTypes.query_type = v.query_type;
    SearchTypes.order = v.order;
    SearchTypes.limit = v.limit;
  } : SearchTypes.query_request)

let rec decode_file json =
  let v = default_file_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "additions" -> 
      let json = Js.Dict.unsafeGet json "additions" in
      v.additions <- Pbrt_bs.int32 json "file" "additions"
    | "deletions" -> 
      let json = Js.Dict.unsafeGet json "deletions" in
      v.deletions <- Pbrt_bs.int32 json "file" "deletions"
    | "path" -> 
      let json = Js.Dict.unsafeGet json "path" in
      v.path <- Pbrt_bs.string json "file" "path"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    SearchTypes.additions = v.additions;
    SearchTypes.deletions = v.deletions;
    SearchTypes.path = v.path;
  } : SearchTypes.file)

let rec decode_commit json =
  let v = default_commit_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "sha" -> 
      let json = Js.Dict.unsafeGet json "sha" in
      v.sha <- Pbrt_bs.string json "commit" "sha"
    | "title" -> 
      let json = Js.Dict.unsafeGet json "title" in
      v.title <- Pbrt_bs.string json "commit" "title"
    | "author" -> 
      let json = Js.Dict.unsafeGet json "author" in
      v.author <- Pbrt_bs.string json "commit" "author"
    | "authored_at" -> 
      let json = Js.Dict.unsafeGet json "authored_at" in
      v.authored_at <- Some ((TimestampBs.decode_timestamp (Pbrt_bs.string json "commit" "authored_at")))
    | "committer" -> 
      let json = Js.Dict.unsafeGet json "committer" in
      v.committer <- Pbrt_bs.string json "commit" "committer"
    | "committed_at" -> 
      let json = Js.Dict.unsafeGet json "committed_at" in
      v.committed_at <- Some ((TimestampBs.decode_timestamp (Pbrt_bs.string json "commit" "committed_at")))
    | "additions" -> 
      let json = Js.Dict.unsafeGet json "additions" in
      v.additions <- Pbrt_bs.int32 json "commit" "additions"
    | "deletions" -> 
      let json = Js.Dict.unsafeGet json "deletions" in
      v.deletions <- Pbrt_bs.int32 json "commit" "deletions"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    SearchTypes.sha = v.sha;
    SearchTypes.title = v.title;
    SearchTypes.author = v.author;
    SearchTypes.authored_at = v.authored_at;
    SearchTypes.committer = v.committer;
    SearchTypes.committed_at = v.committed_at;
    SearchTypes.additions = v.additions;
    SearchTypes.deletions = v.deletions;
  } : SearchTypes.commit)

let rec decode_change_merged_by_m json =
  let keys = Js.Dict.keys json in
  let rec loop = function 
    | -1 -> Pbrt_bs.E.malformed_variant "change_merged_by_m"
    | i -> 
      begin match Array.unsafe_get keys i with
      | "merged_by" -> 
        let json = Js.Dict.unsafeGet json "merged_by" in
        (SearchTypes.Merged_by (Pbrt_bs.string json "change_merged_by_m" "Merged_by") : SearchTypes.change_merged_by_m)
      
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
    | "change_id" -> 
      let json = Js.Dict.unsafeGet json "change_id" in
      v.change_id <- Pbrt_bs.string json "change" "change_id"
    | "author" -> 
      let json = Js.Dict.unsafeGet json "author" in
      v.author <- Pbrt_bs.string json "change" "author"
    | "title" -> 
      let json = Js.Dict.unsafeGet json "title" in
      v.title <- Pbrt_bs.string json "change" "title"
    | "url" -> 
      let json = Js.Dict.unsafeGet json "url" in
      v.url <- Pbrt_bs.string json "change" "url"
    | "repository_fullname" -> 
      let json = Js.Dict.unsafeGet json "repository_fullname" in
      v.repository_fullname <- Pbrt_bs.string json "change" "repository_fullname"
    | "state" -> 
      let json = Js.Dict.unsafeGet json "state" in
      v.state <- Pbrt_bs.string json "change" "state"
    | "branch" -> 
      let json = Js.Dict.unsafeGet json "branch" in
      v.branch <- Pbrt_bs.string json "change" "branch"
    | "target_branch" -> 
      let json = Js.Dict.unsafeGet json "target_branch" in
      v.target_branch <- Pbrt_bs.string json "change" "target_branch"
    | "created_at" -> 
      let json = Js.Dict.unsafeGet json "created_at" in
      v.created_at <- Some ((TimestampBs.decode_timestamp (Pbrt_bs.string json "change" "created_at")))
    | "updated_at" -> 
      let json = Js.Dict.unsafeGet json "updated_at" in
      v.updated_at <- Some ((TimestampBs.decode_timestamp (Pbrt_bs.string json "change" "updated_at")))
    | "merged_at" -> 
      let json = Js.Dict.unsafeGet json "merged_at" in
      v.merged_at <- Some ((TimestampBs.decode_timestamp (Pbrt_bs.string json "change" "merged_at")))
    | "merged_by" -> 
      let json = Js.Dict.unsafeGet json "merged_by" in
      v.merged_by_m <- Merged_by (Pbrt_bs.string json "change" "merged_by_m")
    | "text" -> 
      let json = Js.Dict.unsafeGet json "text" in
      v.text <- Pbrt_bs.string json "change" "text"
    | "additions" -> 
      let json = Js.Dict.unsafeGet json "additions" in
      v.additions <- Pbrt_bs.int32 json "change" "additions"
    | "deletions" -> 
      let json = Js.Dict.unsafeGet json "deletions" in
      v.deletions <- Pbrt_bs.int32 json "change" "deletions"
    | "approval" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "approval" in 
        Pbrt_bs.array_ a "change" "approval"
      in
      v.approval <- Array.map (fun json -> 
        Pbrt_bs.string json "change" "approval"
      ) a |> Array.to_list;
    end
    | "assignees" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "assignees" in 
        Pbrt_bs.array_ a "change" "assignees"
      in
      v.assignees <- Array.map (fun json -> 
        Pbrt_bs.string json "change" "assignees"
      ) a |> Array.to_list;
    end
    | "labels" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "labels" in 
        Pbrt_bs.array_ a "change" "labels"
      in
      v.labels <- Array.map (fun json -> 
        Pbrt_bs.string json "change" "labels"
      ) a |> Array.to_list;
    end
    | "draft" -> 
      let json = Js.Dict.unsafeGet json "draft" in
      v.draft <- Pbrt_bs.bool json "change" "draft"
    | "mergeable" -> 
      let json = Js.Dict.unsafeGet json "mergeable" in
      v.mergeable <- Pbrt_bs.bool json "change" "mergeable"
    | "changed_files" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "changed_files" in 
        Pbrt_bs.array_ a "change" "changed_files"
      in
      v.changed_files <- Array.map (fun json -> 
        (decode_file (Pbrt_bs.object_ json "change" "changed_files"))
      ) a |> Array.to_list;
    end
    | "changed_filesCount" -> 
      let json = Js.Dict.unsafeGet json "changed_filesCount" in
      v.changed_files_count <- Pbrt_bs.int32 json "change" "changed_files_count"
    | "commits" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "commits" in 
        Pbrt_bs.array_ a "change" "commits"
      in
      v.commits <- Array.map (fun json -> 
        (decode_commit (Pbrt_bs.object_ json "change" "commits"))
      ) a |> Array.to_list;
    end
    | "commits_count" -> 
      let json = Js.Dict.unsafeGet json "commits_count" in
      v.commits_count <- Pbrt_bs.int32 json "change" "commits_count"
    | "task_data" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "task_data" in 
        Pbrt_bs.array_ a "change" "task_data"
      in
      v.task_data <- Array.map (fun json -> 
        (TaskDataBs.decode_task_data (Pbrt_bs.object_ json "change" "task_data"))
      ) a |> Array.to_list;
    end
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    SearchTypes.change_id = v.change_id;
    SearchTypes.author = v.author;
    SearchTypes.title = v.title;
    SearchTypes.url = v.url;
    SearchTypes.repository_fullname = v.repository_fullname;
    SearchTypes.state = v.state;
    SearchTypes.branch = v.branch;
    SearchTypes.target_branch = v.target_branch;
    SearchTypes.created_at = v.created_at;
    SearchTypes.updated_at = v.updated_at;
    SearchTypes.merged_at = v.merged_at;
    SearchTypes.merged_by_m = v.merged_by_m;
    SearchTypes.text = v.text;
    SearchTypes.additions = v.additions;
    SearchTypes.deletions = v.deletions;
    SearchTypes.approval = v.approval;
    SearchTypes.assignees = v.assignees;
    SearchTypes.labels = v.labels;
    SearchTypes.draft = v.draft;
    SearchTypes.mergeable = v.mergeable;
    SearchTypes.changed_files = v.changed_files;
    SearchTypes.changed_files_count = v.changed_files_count;
    SearchTypes.commits = v.commits;
    SearchTypes.commits_count = v.commits_count;
    SearchTypes.task_data = v.task_data;
  } : SearchTypes.change)

let rec decode_changes json =
  let v = default_changes_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "changes" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "changes" in 
        Pbrt_bs.array_ a "changes" "changes"
      in
      v.changes <- Array.map (fun json -> 
        (decode_change (Pbrt_bs.object_ json "changes" "changes"))
      ) a |> Array.to_list;
    end
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    SearchTypes.changes = v.changes;
  } : SearchTypes.changes)

let rec decode_repo_summary json =
  let v = default_repo_summary_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "fullname" -> 
      let json = Js.Dict.unsafeGet json "fullname" in
      v.fullname <- Pbrt_bs.string json "repo_summary" "fullname"
    | "total_changes" -> 
      let json = Js.Dict.unsafeGet json "total_changes" in
      v.total_changes <- Pbrt_bs.int32 json "repo_summary" "total_changes"
    | "abandoned_changes" -> 
      let json = Js.Dict.unsafeGet json "abandoned_changes" in
      v.abandoned_changes <- Pbrt_bs.int32 json "repo_summary" "abandoned_changes"
    | "merged_changes" -> 
      let json = Js.Dict.unsafeGet json "merged_changes" in
      v.merged_changes <- Pbrt_bs.int32 json "repo_summary" "merged_changes"
    | "open_changes" -> 
      let json = Js.Dict.unsafeGet json "open_changes" in
      v.open_changes <- Pbrt_bs.int32 json "repo_summary" "open_changes"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    SearchTypes.fullname = v.fullname;
    SearchTypes.total_changes = v.total_changes;
    SearchTypes.abandoned_changes = v.abandoned_changes;
    SearchTypes.merged_changes = v.merged_changes;
    SearchTypes.open_changes = v.open_changes;
  } : SearchTypes.repo_summary)

let rec decode_repos_summary json =
  let v = default_repos_summary_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "repository_summary" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "repository_summary" in 
        Pbrt_bs.array_ a "repos_summary" "repository_summary"
      in
      v.repository_summary <- Array.map (fun json -> 
        (decode_repo_summary (Pbrt_bs.object_ json "repos_summary" "repository_summary"))
      ) a |> Array.to_list;
    end
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    SearchTypes.repository_summary = v.repository_summary;
  } : SearchTypes.repos_summary)

let rec decode_query_response json =
  let keys = Js.Dict.keys json in
  let rec loop = function 
    | -1 -> Pbrt_bs.E.malformed_variant "query_response"
    | i -> 
      begin match Array.unsafe_get keys i with
      | "error" -> 
        let json = Js.Dict.unsafeGet json "error" in
        (SearchTypes.Error ((decode_query_error (Pbrt_bs.object_ json "query_response" "Error"))) : SearchTypes.query_response)
      | "changes" -> 
        let json = Js.Dict.unsafeGet json "changes" in
        (SearchTypes.Changes ((decode_changes (Pbrt_bs.object_ json "query_response" "Changes"))) : SearchTypes.query_response)
      | "repos_summary" -> 
        let json = Js.Dict.unsafeGet json "repos_summary" in
        (SearchTypes.Repos_summary ((decode_repos_summary (Pbrt_bs.object_ json "query_response" "Repos_summary"))) : SearchTypes.query_response)
      
      | _ -> loop (i - 1)
      end
  in
  loop (Array.length keys - 1)

let rec decode_changes_histos_event json =
  let v = default_changes_histos_event_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "doc_count" -> 
      let json = Js.Dict.unsafeGet json "doc_count" in
      v.doc_count <- Pbrt_bs.int32 json "changes_histos_event" "doc_count"
    | "key" -> 
      let json = Js.Dict.unsafeGet json "key" in
      v.key <- Pbrt_bs.int64 json "changes_histos_event" "key"
    | "key_asString" -> 
      let json = Js.Dict.unsafeGet json "key_asString" in
      v.key_as_string <- Pbrt_bs.string json "changes_histos_event" "key_as_string"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    SearchTypes.doc_count = v.doc_count;
    SearchTypes.key = v.key;
    SearchTypes.key_as_string = v.key_as_string;
  } : SearchTypes.changes_histos_event)

let rec decode_changes_histos json =
  let v = default_changes_histos_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "change_abandonedEvent" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "change_abandonedEvent" in 
        Pbrt_bs.array_ a "changes_histos" "change_abandoned_event"
      in
      v.change_abandoned_event <- Array.map (fun json -> 
        (decode_changes_histos_event (Pbrt_bs.object_ json "changes_histos" "change_abandoned_event"))
      ) a |> Array.to_list;
    end
    | "change_commitForcePushedEvent" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "change_commitForcePushedEvent" in 
        Pbrt_bs.array_ a "changes_histos" "change_commit_force_pushed_event"
      in
      v.change_commit_force_pushed_event <- Array.map (fun json -> 
        (decode_changes_histos_event (Pbrt_bs.object_ json "changes_histos" "change_commit_force_pushed_event"))
      ) a |> Array.to_list;
    end
    | "change_commitPushedEvent" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "change_commitPushedEvent" in 
        Pbrt_bs.array_ a "changes_histos" "change_commit_pushed_event"
      in
      v.change_commit_pushed_event <- Array.map (fun json -> 
        (decode_changes_histos_event (Pbrt_bs.object_ json "changes_histos" "change_commit_pushed_event"))
      ) a |> Array.to_list;
    end
    | "change_createdEvent" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "change_createdEvent" in 
        Pbrt_bs.array_ a "changes_histos" "change_created_event"
      in
      v.change_created_event <- Array.map (fun json -> 
        (decode_changes_histos_event (Pbrt_bs.object_ json "changes_histos" "change_created_event"))
      ) a |> Array.to_list;
    end
    | "change_mergedEvent" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "change_mergedEvent" in 
        Pbrt_bs.array_ a "changes_histos" "change_merged_event"
      in
      v.change_merged_event <- Array.map (fun json -> 
        (decode_changes_histos_event (Pbrt_bs.object_ json "changes_histos" "change_merged_event"))
      ) a |> Array.to_list;
    end
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    SearchTypes.change_abandoned_event = v.change_abandoned_event;
    SearchTypes.change_commit_force_pushed_event = v.change_commit_force_pushed_event;
    SearchTypes.change_commit_pushed_event = v.change_commit_pushed_event;
    SearchTypes.change_created_event = v.change_created_event;
    SearchTypes.change_merged_event = v.change_merged_event;
  } : SearchTypes.changes_histos)

let rec decode_changes_lifecycle_event json =
  let v = default_changes_lifecycle_event_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "authors_count" -> 
      let json = Js.Dict.unsafeGet json "authors_count" in
      v.authors_count <- Pbrt_bs.int32 json "changes_lifecycle_event" "authors_count"
    | "events_count" -> 
      let json = Js.Dict.unsafeGet json "events_count" in
      v.events_count <- Pbrt_bs.int32 json "changes_lifecycle_event" "events_count"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    SearchTypes.authors_count = v.authors_count;
    SearchTypes.events_count = v.events_count;
  } : SearchTypes.changes_lifecycle_event)

let rec decode_changes_lifecycle_ratios json =
  let v = default_changes_lifecycle_ratios_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "abandoned" -> 
      let json = Js.Dict.unsafeGet json "abandoned" in
      v.abandoned <- Pbrt_bs.float json "changes_lifecycle_ratios" "abandoned"
    | "iterations" -> 
      let json = Js.Dict.unsafeGet json "iterations" in
      v.iterations <- Pbrt_bs.float json "changes_lifecycle_ratios" "iterations"
    | "merged" -> 
      let json = Js.Dict.unsafeGet json "merged" in
      v.merged <- Pbrt_bs.float json "changes_lifecycle_ratios" "merged"
    | "self_merged" -> 
      let json = Js.Dict.unsafeGet json "self_merged" in
      v.self_merged <- Pbrt_bs.float json "changes_lifecycle_ratios" "self_merged"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    SearchTypes.abandoned = v.abandoned;
    SearchTypes.iterations = v.iterations;
    SearchTypes.merged = v.merged;
    SearchTypes.self_merged = v.self_merged;
  } : SearchTypes.changes_lifecycle_ratios)

let rec decode_changes_lifecycle json =
  let v = default_changes_lifecycle_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "change_commitForcePushedEvent" -> 
      let json = Js.Dict.unsafeGet json "change_commitForcePushedEvent" in
      v.change_commit_force_pushed_event <- Some ((decode_changes_lifecycle_event (Pbrt_bs.object_ json "changes_lifecycle" "change_commit_force_pushed_event")))
    | "change_commitPushedEvent" -> 
      let json = Js.Dict.unsafeGet json "change_commitPushedEvent" in
      v.change_commit_pushed_event <- Some ((decode_changes_lifecycle_event (Pbrt_bs.object_ json "changes_lifecycle" "change_commit_pushed_event")))
    | "change_createdEvent" -> 
      let json = Js.Dict.unsafeGet json "change_createdEvent" in
      v.change_created_event <- Some ((decode_changes_lifecycle_event (Pbrt_bs.object_ json "changes_lifecycle" "change_created_event")))
    | "abandoned" -> 
      let json = Js.Dict.unsafeGet json "abandoned" in
      v.abandoned <- Pbrt_bs.int32 json "changes_lifecycle" "abandoned"
    | "commits" -> 
      let json = Js.Dict.unsafeGet json "commits" in
      v.commits <- Pbrt_bs.float json "changes_lifecycle" "commits"
    | "duration" -> 
      let json = Js.Dict.unsafeGet json "duration" in
      v.duration <- Pbrt_bs.float json "changes_lifecycle" "duration"
    | "duration_variability" -> 
      let json = Js.Dict.unsafeGet json "duration_variability" in
      v.duration_variability <- Pbrt_bs.float json "changes_lifecycle" "duration_variability"
    | "histos" -> 
      let json = Js.Dict.unsafeGet json "histos" in
      v.histos <- Some ((decode_changes_histos (Pbrt_bs.object_ json "changes_lifecycle" "histos")))
    | "merged" -> 
      let json = Js.Dict.unsafeGet json "merged" in
      v.merged <- Pbrt_bs.int32 json "changes_lifecycle" "merged"
    | "opened" -> 
      let json = Js.Dict.unsafeGet json "opened" in
      v.opened <- Pbrt_bs.int32 json "changes_lifecycle" "opened"
    | "ratios" -> 
      let json = Js.Dict.unsafeGet json "ratios" in
      v.ratios <- Some ((decode_changes_lifecycle_ratios (Pbrt_bs.object_ json "changes_lifecycle" "ratios")))
    | "self_merged" -> 
      let json = Js.Dict.unsafeGet json "self_merged" in
      v.self_merged <- Pbrt_bs.int32 json "changes_lifecycle" "self_merged"
    | "tests" -> 
      let json = Js.Dict.unsafeGet json "tests" in
      v.tests <- Pbrt_bs.float json "changes_lifecycle" "tests"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    SearchTypes.change_commit_force_pushed_event = v.change_commit_force_pushed_event;
    SearchTypes.change_commit_pushed_event = v.change_commit_pushed_event;
    SearchTypes.change_created_event = v.change_created_event;
    SearchTypes.abandoned = v.abandoned;
    SearchTypes.commits = v.commits;
    SearchTypes.duration = v.duration;
    SearchTypes.duration_variability = v.duration_variability;
    SearchTypes.histos = v.histos;
    SearchTypes.merged = v.merged;
    SearchTypes.opened = v.opened;
    SearchTypes.ratios = v.ratios;
    SearchTypes.self_merged = v.self_merged;
    SearchTypes.tests = v.tests;
  } : SearchTypes.changes_lifecycle)

let rec encode_search_suggestions_request (v:SearchTypes.search_suggestions_request) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "index" (Js.Json.string v.SearchTypes.index);
  json

let rec encode_search_suggestions_response (v:SearchTypes.search_suggestions_response) = 
  let json = Js.Dict.empty () in
  let a = v.SearchTypes.task_types |> Array.of_list |> Array.map Js.Json.string in
  Js.Dict.set json "task_types" (Js.Json.array a);
  let a = v.SearchTypes.authors |> Array.of_list |> Array.map Js.Json.string in
  Js.Dict.set json "authors" (Js.Json.array a);
  let a = v.SearchTypes.approvals |> Array.of_list |> Array.map Js.Json.string in
  Js.Dict.set json "approvals" (Js.Json.array a);
  let a = v.SearchTypes.priorities |> Array.of_list |> Array.map Js.Json.string in
  Js.Dict.set json "priorities" (Js.Json.array a);
  let a = v.SearchTypes.severities |> Array.of_list |> Array.map Js.Json.string in
  Js.Dict.set json "severities" (Js.Json.array a);
  json

let rec encode_fields_request (v:SearchTypes.fields_request) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "version" (Js.Json.string v.SearchTypes.version);
  json

let rec encode_field_type (v:SearchTypes.field_type) : string = 
  match v with
  | SearchTypes.Field_date -> "FIELD_DATE"
  | SearchTypes.Field_number -> "FIELD_NUMBER"
  | SearchTypes.Field_text -> "FIELD_TEXT"
  | SearchTypes.Field_bool -> "FIELD_BOOL"
  | SearchTypes.Field_regex -> "FIELD_REGEX"

let rec encode_field (v:SearchTypes.field) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "name" (Js.Json.string v.SearchTypes.name);
  Js.Dict.set json "description" (Js.Json.string v.SearchTypes.description);
  Js.Dict.set json "type" (Js.Json.string (encode_field_type v.SearchTypes.type_));
  json

let rec encode_fields_response (v:SearchTypes.fields_response) = 
  let json = Js.Dict.empty () in
  begin (* fields field *)
    let (fields':Js.Json.t) =
      v.SearchTypes.fields
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_field |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "fields" fields';
  end;
  json

let rec encode_query_error (v:SearchTypes.query_error) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "message" (Js.Json.string v.SearchTypes.message);
  Js.Dict.set json "position" (Js.Json.number (Int32.to_float v.SearchTypes.position));
  json

let rec encode_order_direction (v:SearchTypes.order_direction) : string = 
  match v with
  | SearchTypes.Asc -> "ASC"
  | SearchTypes.Desc -> "DESC"

let rec encode_order (v:SearchTypes.order) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "field" (Js.Json.string v.SearchTypes.field);
  Js.Dict.set json "direction" (Js.Json.string (encode_order_direction v.SearchTypes.direction));
  json

let rec encode_query_request_query_type (v:SearchTypes.query_request_query_type) : string = 
  match v with
  | SearchTypes.Query_change -> "QUERY_CHANGE"
  | SearchTypes.Query_change_lifecycle -> "QUERY_CHANGE_LIFECYCLE"
  | SearchTypes.Query_repos_summary -> "QUERY_REPOS_SUMMARY"

let rec encode_query_request (v:SearchTypes.query_request) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "index" (Js.Json.string v.SearchTypes.index);
  Js.Dict.set json "username" (Js.Json.string v.SearchTypes.username);
  Js.Dict.set json "query" (Js.Json.string v.SearchTypes.query);
  Js.Dict.set json "query_type" (Js.Json.string (encode_query_request_query_type v.SearchTypes.query_type));
  begin match v.SearchTypes.order with
  | None -> ()
  | Some v ->
    begin (* order field *)
      let json' = encode_order v in
      Js.Dict.set json "order" (Js.Json.object_ json');
    end;
  end;
  Js.Dict.set json "limit" (Js.Json.number (Int32.to_float v.SearchTypes.limit));
  json

let rec encode_file (v:SearchTypes.file) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "additions" (Js.Json.number (Int32.to_float v.SearchTypes.additions));
  Js.Dict.set json "deletions" (Js.Json.number (Int32.to_float v.SearchTypes.deletions));
  Js.Dict.set json "path" (Js.Json.string v.SearchTypes.path);
  json

let rec encode_commit (v:SearchTypes.commit) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "sha" (Js.Json.string v.SearchTypes.sha);
  Js.Dict.set json "title" (Js.Json.string v.SearchTypes.title);
  Js.Dict.set json "author" (Js.Json.string v.SearchTypes.author);
  begin match v.SearchTypes.authored_at with
  | None -> ()
  | Some v ->
    begin (* authoredAt field *)
      let json' = TimestampBs.encode_timestamp v in
      Js.Dict.set json "authored_at" (Js.Json.string json');
    end;
  end;
  Js.Dict.set json "committer" (Js.Json.string v.SearchTypes.committer);
  begin match v.SearchTypes.committed_at with
  | None -> ()
  | Some v ->
    begin (* committedAt field *)
      let json' = TimestampBs.encode_timestamp v in
      Js.Dict.set json "committed_at" (Js.Json.string json');
    end;
  end;
  Js.Dict.set json "additions" (Js.Json.number (Int32.to_float v.SearchTypes.additions));
  Js.Dict.set json "deletions" (Js.Json.number (Int32.to_float v.SearchTypes.deletions));
  json

let rec encode_change_merged_by_m (v:SearchTypes.change_merged_by_m) = 
  let json = Js.Dict.empty () in
  begin match v with
  | SearchTypes.Merged_by v ->
    Js.Dict.set json "merged_by" (Js.Json.string v);
  end;
  json

and encode_change (v:SearchTypes.change) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "change_id" (Js.Json.string v.SearchTypes.change_id);
  Js.Dict.set json "author" (Js.Json.string v.SearchTypes.author);
  Js.Dict.set json "title" (Js.Json.string v.SearchTypes.title);
  Js.Dict.set json "url" (Js.Json.string v.SearchTypes.url);
  Js.Dict.set json "repository_fullname" (Js.Json.string v.SearchTypes.repository_fullname);
  Js.Dict.set json "state" (Js.Json.string v.SearchTypes.state);
  Js.Dict.set json "branch" (Js.Json.string v.SearchTypes.branch);
  Js.Dict.set json "target_branch" (Js.Json.string v.SearchTypes.target_branch);
  begin match v.SearchTypes.created_at with
  | None -> ()
  | Some v ->
    begin (* createdAt field *)
      let json' = TimestampBs.encode_timestamp v in
      Js.Dict.set json "created_at" (Js.Json.string json');
    end;
  end;
  begin match v.SearchTypes.updated_at with
  | None -> ()
  | Some v ->
    begin (* updatedAt field *)
      let json' = TimestampBs.encode_timestamp v in
      Js.Dict.set json "updated_at" (Js.Json.string json');
    end;
  end;
  begin match v.SearchTypes.merged_at with
  | None -> ()
  | Some v ->
    begin (* mergedAt field *)
      let json' = TimestampBs.encode_timestamp v in
      Js.Dict.set json "merged_at" (Js.Json.string json');
    end;
  end;
  begin match v.SearchTypes.merged_by_m with
    | Merged_by v ->
      Js.Dict.set json "merged_by" (Js.Json.string v);
  end; (* match v.merged_by_m *)
  Js.Dict.set json "text" (Js.Json.string v.SearchTypes.text);
  Js.Dict.set json "additions" (Js.Json.number (Int32.to_float v.SearchTypes.additions));
  Js.Dict.set json "deletions" (Js.Json.number (Int32.to_float v.SearchTypes.deletions));
  let a = v.SearchTypes.approval |> Array.of_list |> Array.map Js.Json.string in
  Js.Dict.set json "approval" (Js.Json.array a);
  let a = v.SearchTypes.assignees |> Array.of_list |> Array.map Js.Json.string in
  Js.Dict.set json "assignees" (Js.Json.array a);
  let a = v.SearchTypes.labels |> Array.of_list |> Array.map Js.Json.string in
  Js.Dict.set json "labels" (Js.Json.array a);
  Js.Dict.set json "draft" (Js.Json.boolean v.SearchTypes.draft);
  Js.Dict.set json "mergeable" (Js.Json.boolean v.SearchTypes.mergeable);
  begin (* changedFiles field *)
    let (changed_files':Js.Json.t) =
      v.SearchTypes.changed_files
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_file |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "changed_files" changed_files';
  end;
  Js.Dict.set json "changed_filesCount" (Js.Json.number (Int32.to_float v.SearchTypes.changed_files_count));
  begin (* commits field *)
    let (commits':Js.Json.t) =
      v.SearchTypes.commits
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_commit |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "commits" commits';
  end;
  Js.Dict.set json "commits_count" (Js.Json.number (Int32.to_float v.SearchTypes.commits_count));
  begin (* taskData field *)
    let (task_data':Js.Json.t) =
      v.SearchTypes.task_data
      |> Array.of_list
      |> Array.map (fun v ->
        v |> TaskDataBs.encode_task_data |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "task_data" task_data';
  end;
  json

let rec encode_changes (v:SearchTypes.changes) = 
  let json = Js.Dict.empty () in
  begin (* changes field *)
    let (changes':Js.Json.t) =
      v.SearchTypes.changes
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_change |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "changes" changes';
  end;
  json

let rec encode_repo_summary (v:SearchTypes.repo_summary) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "fullname" (Js.Json.string v.SearchTypes.fullname);
  Js.Dict.set json "total_changes" (Js.Json.number (Int32.to_float v.SearchTypes.total_changes));
  Js.Dict.set json "abandoned_changes" (Js.Json.number (Int32.to_float v.SearchTypes.abandoned_changes));
  Js.Dict.set json "merged_changes" (Js.Json.number (Int32.to_float v.SearchTypes.merged_changes));
  Js.Dict.set json "open_changes" (Js.Json.number (Int32.to_float v.SearchTypes.open_changes));
  json

let rec encode_repos_summary (v:SearchTypes.repos_summary) = 
  let json = Js.Dict.empty () in
  begin (* repositorySummary field *)
    let (repository_summary':Js.Json.t) =
      v.SearchTypes.repository_summary
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_repo_summary |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "repository_summary" repository_summary';
  end;
  json

let rec encode_query_response (v:SearchTypes.query_response) = 
  let json = Js.Dict.empty () in
  begin match v with
  | SearchTypes.Error v ->
    begin (* error field *)
      let json' = encode_query_error v in
      Js.Dict.set json "error" (Js.Json.object_ json');
    end;
  | SearchTypes.Changes v ->
    begin (* changes field *)
      let json' = encode_changes v in
      Js.Dict.set json "changes" (Js.Json.object_ json');
    end;
  | SearchTypes.Repos_summary v ->
    begin (* reposSummary field *)
      let json' = encode_repos_summary v in
      Js.Dict.set json "repos_summary" (Js.Json.object_ json');
    end;
  end;
  json

let rec encode_changes_histos_event (v:SearchTypes.changes_histos_event) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "doc_count" (Js.Json.number (Int32.to_float v.SearchTypes.doc_count));
  Js.Dict.set json "key" (Js.Json.string (Int64.to_string v.SearchTypes.key));
  Js.Dict.set json "key_asString" (Js.Json.string v.SearchTypes.key_as_string);
  json

let rec encode_changes_histos (v:SearchTypes.changes_histos) = 
  let json = Js.Dict.empty () in
  begin (* changeAbandonedEvent field *)
    let (change_abandoned_event':Js.Json.t) =
      v.SearchTypes.change_abandoned_event
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_changes_histos_event |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "change_abandonedEvent" change_abandoned_event';
  end;
  begin (* changeCommitForcePushedEvent field *)
    let (change_commit_force_pushed_event':Js.Json.t) =
      v.SearchTypes.change_commit_force_pushed_event
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_changes_histos_event |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "change_commitForcePushedEvent" change_commit_force_pushed_event';
  end;
  begin (* changeCommitPushedEvent field *)
    let (change_commit_pushed_event':Js.Json.t) =
      v.SearchTypes.change_commit_pushed_event
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_changes_histos_event |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "change_commitPushedEvent" change_commit_pushed_event';
  end;
  begin (* changeCreatedEvent field *)
    let (change_created_event':Js.Json.t) =
      v.SearchTypes.change_created_event
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_changes_histos_event |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "change_createdEvent" change_created_event';
  end;
  begin (* changeMergedEvent field *)
    let (change_merged_event':Js.Json.t) =
      v.SearchTypes.change_merged_event
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_changes_histos_event |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "change_mergedEvent" change_merged_event';
  end;
  json

let rec encode_changes_lifecycle_event (v:SearchTypes.changes_lifecycle_event) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "authors_count" (Js.Json.number (Int32.to_float v.SearchTypes.authors_count));
  Js.Dict.set json "events_count" (Js.Json.number (Int32.to_float v.SearchTypes.events_count));
  json

let rec encode_changes_lifecycle_ratios (v:SearchTypes.changes_lifecycle_ratios) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "abandoned" (Js.Json.number v.SearchTypes.abandoned);
  Js.Dict.set json "iterations" (Js.Json.number v.SearchTypes.iterations);
  Js.Dict.set json "merged" (Js.Json.number v.SearchTypes.merged);
  Js.Dict.set json "self_merged" (Js.Json.number v.SearchTypes.self_merged);
  json

let rec encode_changes_lifecycle (v:SearchTypes.changes_lifecycle) = 
  let json = Js.Dict.empty () in
  begin match v.SearchTypes.change_commit_force_pushed_event with
  | None -> ()
  | Some v ->
    begin (* changeCommitForcePushedEvent field *)
      let json' = encode_changes_lifecycle_event v in
      Js.Dict.set json "change_commitForcePushedEvent" (Js.Json.object_ json');
    end;
  end;
  begin match v.SearchTypes.change_commit_pushed_event with
  | None -> ()
  | Some v ->
    begin (* changeCommitPushedEvent field *)
      let json' = encode_changes_lifecycle_event v in
      Js.Dict.set json "change_commitPushedEvent" (Js.Json.object_ json');
    end;
  end;
  begin match v.SearchTypes.change_created_event with
  | None -> ()
  | Some v ->
    begin (* changeCreatedEvent field *)
      let json' = encode_changes_lifecycle_event v in
      Js.Dict.set json "change_createdEvent" (Js.Json.object_ json');
    end;
  end;
  Js.Dict.set json "abandoned" (Js.Json.number (Int32.to_float v.SearchTypes.abandoned));
  Js.Dict.set json "commits" (Js.Json.number v.SearchTypes.commits);
  Js.Dict.set json "duration" (Js.Json.number v.SearchTypes.duration);
  Js.Dict.set json "duration_variability" (Js.Json.number v.SearchTypes.duration_variability);
  begin match v.SearchTypes.histos with
  | None -> ()
  | Some v ->
    begin (* histos field *)
      let json' = encode_changes_histos v in
      Js.Dict.set json "histos" (Js.Json.object_ json');
    end;
  end;
  Js.Dict.set json "merged" (Js.Json.number (Int32.to_float v.SearchTypes.merged));
  Js.Dict.set json "opened" (Js.Json.number (Int32.to_float v.SearchTypes.opened));
  begin match v.SearchTypes.ratios with
  | None -> ()
  | Some v ->
    begin (* ratios field *)
      let json' = encode_changes_lifecycle_ratios v in
      Js.Dict.set json "ratios" (Js.Json.object_ json');
    end;
  end;
  Js.Dict.set json "self_merged" (Js.Json.number (Int32.to_float v.SearchTypes.self_merged));
  Js.Dict.set json "tests" (Js.Json.number v.SearchTypes.tests);
  json
