[@@@ocaml.warning "-27-30-39"]

type task_data_mutable = {
  mutable updated_at : TimestampTypes.timestamp option;
  mutable change_url : string;
  mutable ttype : string list;
  mutable tid : string;
  mutable url : string;
  mutable title : string;
  mutable severity : string;
  mutable priority : string;
  mutable score : int32;
  mutable prefix : string;
}

let default_task_data_mutable () : task_data_mutable = {
  updated_at = None;
  change_url = "";
  ttype = [];
  tid = "";
  url = "";
  title = "";
  severity = "";
  priority = "";
  score = 0l;
  prefix = "";
}

type suggestions_request_mutable = {
  mutable index : string;
}

let default_suggestions_request_mutable () : suggestions_request_mutable = {
  index = "";
}

type suggestions_response_mutable = {
  mutable task_types : string list;
  mutable authors : string list;
  mutable approvals : string list;
  mutable priorities : string list;
  mutable severities : string list;
  mutable projects : string list;
  mutable groups : string list;
  mutable labels : string list;
}

let default_suggestions_response_mutable () : suggestions_response_mutable = {
  task_types = [];
  authors = [];
  approvals = [];
  priorities = [];
  severities = [];
  projects = [];
  groups = [];
  labels = [];
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

type check_request_mutable = {
  mutable index : string;
  mutable username : string;
  mutable query : string;
}

let default_check_request_mutable () : check_request_mutable = {
  index = "";
  username = "";
  query = "";
}

type author_mutable = {
  mutable muid : string;
  mutable aliases : string list;
  mutable groups : string list;
}

let default_author_mutable () : author_mutable = {
  muid = "";
  aliases = [];
  groups = [];
}

type author_request_mutable = {
  mutable index : string;
  mutable query : string;
}

let default_author_request_mutable () : author_request_mutable = {
  index = "";
  query = "";
}

type author_response_mutable = {
  mutable authors : SearchTypes.author list;
}

let default_author_response_mutable () : author_response_mutable = {
  authors = [];
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
  mutable change_id : string;
}

let default_query_request_mutable () : query_request_mutable = {
  index = "";
  username = "";
  query = "";
  query_type = SearchTypes.default_query_request_query_type ();
  order = None;
  limit = 0l;
  change_id = "";
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
  mutable task_data : SearchTypes.task_data list;
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

type ratio_mutable = {
  mutable ratio : float;
}

let default_ratio_mutable () : ratio_mutable = {
  ratio = 0.;
}

type change_event_mutable = {
  mutable id : string;
  mutable type_ : string;
  mutable change_id : string;
  mutable created_at : TimestampTypes.timestamp option;
  mutable on_created_at : TimestampTypes.timestamp option;
  mutable author : string;
  mutable on_author : string;
  mutable branch : string;
}

let default_change_event_mutable () : change_event_mutable = {
  id = "";
  type_ = "";
  change_id = "";
  created_at = None;
  on_created_at = None;
  author = "";
  on_author = "";
  branch = "";
}

type change_and_events_mutable = {
  mutable change : SearchTypes.change option;
  mutable events : SearchTypes.change_event list;
}

let default_change_and_events_mutable () : change_and_events_mutable = {
  change = None;
  events = [];
}

type review_count_mutable = {
  mutable authors_count : int32;
  mutable events_count : int32;
}

let default_review_count_mutable () : review_count_mutable = {
  authors_count = 0l;
  events_count = 0l;
}

type histo_mutable = {
  mutable date : string;
  mutable count : int32;
}

let default_histo_mutable () : histo_mutable = {
  date = "";
  count = 0l;
}

type histo_stat_mutable = {
  mutable histo : SearchTypes.histo list;
}

let default_histo_stat_mutable () : histo_stat_mutable = {
  histo = [];
}

type review_stats_mutable = {
  mutable comment_count : SearchTypes.review_count option;
  mutable review_count : SearchTypes.review_count option;
  mutable comment_delay : int32;
  mutable review_delay : int32;
  mutable comment_histo : SearchTypes.histo list;
  mutable review_histo : SearchTypes.histo list;
}

let default_review_stats_mutable () : review_stats_mutable = {
  comment_count = None;
  review_count = None;
  comment_delay = 0l;
  review_delay = 0l;
  comment_histo = [];
  review_histo = [];
}

type activity_stats_mutable = {
  mutable change_authors : int32;
  mutable comment_authors : int32;
  mutable review_authors : int32;
  mutable comments_histo : SearchTypes.histo list;
  mutable reviews_histo : SearchTypes.histo list;
  mutable changes_histo : SearchTypes.histo list;
}

let default_activity_stats_mutable () : activity_stats_mutable = {
  change_authors = 0l;
  comment_authors = 0l;
  review_authors = 0l;
  comments_histo = [];
  reviews_histo = [];
  changes_histo = [];
}

type repo_summary_mutable = {
  mutable fullname : string;
  mutable created_changes : int32;
  mutable abandoned_changes : int32;
  mutable merged_changes : int32;
  mutable updated_changes : int32;
  mutable open_changes : int32;
}

let default_repo_summary_mutable () : repo_summary_mutable = {
  fullname = "";
  created_changes = 0l;
  abandoned_changes = 0l;
  merged_changes = 0l;
  updated_changes = 0l;
  open_changes = 0l;
}

type repos_summary_mutable = {
  mutable reposum : SearchTypes.repo_summary list;
}

let default_repos_summary_mutable () : repos_summary_mutable = {
  reposum = [];
}

type term_count_mutable = {
  mutable term : string;
  mutable count : int32;
}

let default_term_count_mutable () : term_count_mutable = {
  term = "";
  count = 0l;
}

type terms_count_mutable = {
  mutable termcount : SearchTypes.term_count list;
  mutable total_hits : int32;
}

let default_terms_count_mutable () : terms_count_mutable = {
  termcount = [];
  total_hits = 0l;
}

type author_peer_mutable = {
  mutable author : string;
  mutable peer : string;
  mutable strength : int32;
}

let default_author_peer_mutable () : author_peer_mutable = {
  author = "";
  peer = "";
  strength = 0l;
}

type authors_peers_mutable = {
  mutable author_peer : SearchTypes.author_peer list;
}

let default_authors_peers_mutable () : authors_peers_mutable = {
  author_peer = [];
}

type lifecycle_stats_mutable = {
  mutable created_histo : SearchTypes.histo list;
  mutable updated_histo : SearchTypes.histo list;
  mutable merged_histo : SearchTypes.histo list;
  mutable abandoned_histo : SearchTypes.histo list;
  mutable created : SearchTypes.review_count option;
  mutable abandoned : int32;
  mutable merged : int32;
  mutable self_merged : int32;
  mutable self_merged_ratio : float;
  mutable ttm_mean : float;
  mutable ttm_variability : float;
  mutable updates_of_changes : int32;
  mutable changes_with_tests : float;
  mutable iterations_per_change : float;
  mutable commits_per_change : float;
}

let default_lifecycle_stats_mutable () : lifecycle_stats_mutable = {
  created_histo = [];
  updated_histo = [];
  merged_histo = [];
  abandoned_histo = [];
  created = None;
  abandoned = 0l;
  merged = 0l;
  self_merged = 0l;
  self_merged_ratio = 0.;
  ttm_mean = 0.;
  ttm_variability = 0.;
  updates_of_changes = 0l;
  changes_with_tests = 0.;
  iterations_per_change = 0.;
  commits_per_change = 0.;
}

type changes_tops_mutable = {
  mutable authors : SearchTypes.terms_count option;
  mutable repos : SearchTypes.terms_count option;
  mutable approvals : SearchTypes.terms_count option;
}

let default_changes_tops_mutable () : changes_tops_mutable = {
  authors = None;
  repos = None;
  approvals = None;
}


let rec decode_task_data json =
  let v = default_task_data_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "updated_at" -> 
      let json = Js.Dict.unsafeGet json "updated_at" in
      v.updated_at <- Some ((TimestampBs.decode_timestamp (Pbrt_bs.string json "task_data" "updated_at")))
    | "change_url" -> 
      let json = Js.Dict.unsafeGet json "change_url" in
      v.change_url <- Pbrt_bs.string json "task_data" "change_url"
    | "ttype" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "ttype" in 
        Pbrt_bs.array_ a "task_data" "ttype"
      in
      v.ttype <- Array.map (fun json -> 
        Pbrt_bs.string json "task_data" "ttype"
      ) a |> Array.to_list;
    end
    | "tid" -> 
      let json = Js.Dict.unsafeGet json "tid" in
      v.tid <- Pbrt_bs.string json "task_data" "tid"
    | "url" -> 
      let json = Js.Dict.unsafeGet json "url" in
      v.url <- Pbrt_bs.string json "task_data" "url"
    | "title" -> 
      let json = Js.Dict.unsafeGet json "title" in
      v.title <- Pbrt_bs.string json "task_data" "title"
    | "severity" -> 
      let json = Js.Dict.unsafeGet json "severity" in
      v.severity <- Pbrt_bs.string json "task_data" "severity"
    | "priority" -> 
      let json = Js.Dict.unsafeGet json "priority" in
      v.priority <- Pbrt_bs.string json "task_data" "priority"
    | "score" -> 
      let json = Js.Dict.unsafeGet json "score" in
      v.score <- Pbrt_bs.int32 json "task_data" "score"
    | "prefix" -> 
      let json = Js.Dict.unsafeGet json "prefix" in
      v.prefix <- Pbrt_bs.string json "task_data" "prefix"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    SearchTypes.updated_at = v.updated_at;
    SearchTypes.change_url = v.change_url;
    SearchTypes.ttype = v.ttype;
    SearchTypes.tid = v.tid;
    SearchTypes.url = v.url;
    SearchTypes.title = v.title;
    SearchTypes.severity = v.severity;
    SearchTypes.priority = v.priority;
    SearchTypes.score = v.score;
    SearchTypes.prefix = v.prefix;
  } : SearchTypes.task_data)

let rec decode_suggestions_request json =
  let v = default_suggestions_request_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "index" -> 
      let json = Js.Dict.unsafeGet json "index" in
      v.index <- Pbrt_bs.string json "suggestions_request" "index"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    SearchTypes.index = v.index;
  } : SearchTypes.suggestions_request)

let rec decode_suggestions_response json =
  let v = default_suggestions_response_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "task_types" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "task_types" in 
        Pbrt_bs.array_ a "suggestions_response" "task_types"
      in
      v.task_types <- Array.map (fun json -> 
        Pbrt_bs.string json "suggestions_response" "task_types"
      ) a |> Array.to_list;
    end
    | "authors" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "authors" in 
        Pbrt_bs.array_ a "suggestions_response" "authors"
      in
      v.authors <- Array.map (fun json -> 
        Pbrt_bs.string json "suggestions_response" "authors"
      ) a |> Array.to_list;
    end
    | "approvals" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "approvals" in 
        Pbrt_bs.array_ a "suggestions_response" "approvals"
      in
      v.approvals <- Array.map (fun json -> 
        Pbrt_bs.string json "suggestions_response" "approvals"
      ) a |> Array.to_list;
    end
    | "priorities" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "priorities" in 
        Pbrt_bs.array_ a "suggestions_response" "priorities"
      in
      v.priorities <- Array.map (fun json -> 
        Pbrt_bs.string json "suggestions_response" "priorities"
      ) a |> Array.to_list;
    end
    | "severities" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "severities" in 
        Pbrt_bs.array_ a "suggestions_response" "severities"
      in
      v.severities <- Array.map (fun json -> 
        Pbrt_bs.string json "suggestions_response" "severities"
      ) a |> Array.to_list;
    end
    | "projects" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "projects" in 
        Pbrt_bs.array_ a "suggestions_response" "projects"
      in
      v.projects <- Array.map (fun json -> 
        Pbrt_bs.string json "suggestions_response" "projects"
      ) a |> Array.to_list;
    end
    | "groups" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "groups" in 
        Pbrt_bs.array_ a "suggestions_response" "groups"
      in
      v.groups <- Array.map (fun json -> 
        Pbrt_bs.string json "suggestions_response" "groups"
      ) a |> Array.to_list;
    end
    | "labels" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "labels" in 
        Pbrt_bs.array_ a "suggestions_response" "labels"
      in
      v.labels <- Array.map (fun json -> 
        Pbrt_bs.string json "suggestions_response" "labels"
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
    SearchTypes.projects = v.projects;
    SearchTypes.groups = v.groups;
    SearchTypes.labels = v.labels;
  } : SearchTypes.suggestions_response)

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

let rec decode_check_request json =
  let v = default_check_request_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "index" -> 
      let json = Js.Dict.unsafeGet json "index" in
      v.index <- Pbrt_bs.string json "check_request" "index"
    | "username" -> 
      let json = Js.Dict.unsafeGet json "username" in
      v.username <- Pbrt_bs.string json "check_request" "username"
    | "query" -> 
      let json = Js.Dict.unsafeGet json "query" in
      v.query <- Pbrt_bs.string json "check_request" "query"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    SearchTypes.index = v.index;
    SearchTypes.username = v.username;
    SearchTypes.query = v.query;
  } : SearchTypes.check_request)

let rec decode_check_response json =
  let keys = Js.Dict.keys json in
  let rec loop = function 
    | -1 -> Pbrt_bs.E.malformed_variant "check_response"
    | i -> 
      begin match Array.unsafe_get keys i with
      | "success" -> 
        let json = Js.Dict.unsafeGet json "success" in
        (SearchTypes.Success (Pbrt_bs.string json "check_response" "Success") : SearchTypes.check_response)
      | "error" -> 
        let json = Js.Dict.unsafeGet json "error" in
        (SearchTypes.Error ((decode_query_error (Pbrt_bs.object_ json "check_response" "Error"))) : SearchTypes.check_response)
      
      | _ -> loop (i - 1)
      end
  in
  loop (Array.length keys - 1)

let rec decode_author json =
  let v = default_author_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "muid" -> 
      let json = Js.Dict.unsafeGet json "muid" in
      v.muid <- Pbrt_bs.string json "author" "muid"
    | "aliases" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "aliases" in 
        Pbrt_bs.array_ a "author" "aliases"
      in
      v.aliases <- Array.map (fun json -> 
        Pbrt_bs.string json "author" "aliases"
      ) a |> Array.to_list;
    end
    | "groups" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "groups" in 
        Pbrt_bs.array_ a "author" "groups"
      in
      v.groups <- Array.map (fun json -> 
        Pbrt_bs.string json "author" "groups"
      ) a |> Array.to_list;
    end
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    SearchTypes.muid = v.muid;
    SearchTypes.aliases = v.aliases;
    SearchTypes.groups = v.groups;
  } : SearchTypes.author)

let rec decode_author_request json =
  let v = default_author_request_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "index" -> 
      let json = Js.Dict.unsafeGet json "index" in
      v.index <- Pbrt_bs.string json "author_request" "index"
    | "query" -> 
      let json = Js.Dict.unsafeGet json "query" in
      v.query <- Pbrt_bs.string json "author_request" "query"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    SearchTypes.index = v.index;
    SearchTypes.query = v.query;
  } : SearchTypes.author_request)

let rec decode_author_response json =
  let v = default_author_response_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "authors" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "authors" in 
        Pbrt_bs.array_ a "author_response" "authors"
      in
      v.authors <- Array.map (fun json -> 
        (decode_author (Pbrt_bs.object_ json "author_response" "authors"))
      ) a |> Array.to_list;
    end
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    SearchTypes.authors = v.authors;
  } : SearchTypes.author_response)

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
  | "QUERY_REPOS_SUMMARY" -> (SearchTypes.Query_repos_summary : SearchTypes.query_request_query_type)
  | "QUERY_TOP_AUTHORS_CHANGES_CREATED" -> (SearchTypes.Query_top_authors_changes_created : SearchTypes.query_request_query_type)
  | "QUERY_TOP_AUTHORS_CHANGES_MERGED" -> (SearchTypes.Query_top_authors_changes_merged : SearchTypes.query_request_query_type)
  | "QUERY_TOP_AUTHORS_CHANGES_REVIEWED" -> (SearchTypes.Query_top_authors_changes_reviewed : SearchTypes.query_request_query_type)
  | "QUERY_TOP_AUTHORS_CHANGES_COMMENTED" -> (SearchTypes.Query_top_authors_changes_commented : SearchTypes.query_request_query_type)
  | "QUERY_TOP_REVIEWED_AUTHORS" -> (SearchTypes.Query_top_reviewed_authors : SearchTypes.query_request_query_type)
  | "QUERY_TOP_COMMENTED_AUTHORS" -> (SearchTypes.Query_top_commented_authors : SearchTypes.query_request_query_type)
  | "QUERY_TOP_AUTHORS_PEERS" -> (SearchTypes.Query_top_authors_peers : SearchTypes.query_request_query_type)
  | "QUERY_NEW_CHANGES_AUTHORS" -> (SearchTypes.Query_new_changes_authors : SearchTypes.query_request_query_type)
  | "QUERY_CHANGES_REVIEW_STATS" -> (SearchTypes.Query_changes_review_stats : SearchTypes.query_request_query_type)
  | "QUERY_CHANGES_LIFECYCLE_STATS" -> (SearchTypes.Query_changes_lifecycle_stats : SearchTypes.query_request_query_type)
  | "QUERY_ACTIVE_AUTHORS_STATS" -> (SearchTypes.Query_active_authors_stats : SearchTypes.query_request_query_type)
  | "QUERY_CHANGE_AND_EVENTS" -> (SearchTypes.Query_change_and_events : SearchTypes.query_request_query_type)
  | "QUERY_CHANGES_TOPS" -> (SearchTypes.Query_changes_tops : SearchTypes.query_request_query_type)
  | "QUERY_RATIO_COMMITS_VS_REVIEWS" -> (SearchTypes.Query_ratio_commits_vs_reviews : SearchTypes.query_request_query_type)
  | "QUERY_HISTO_COMMITS" -> (SearchTypes.Query_histo_commits : SearchTypes.query_request_query_type)
  | "QUERY_HISTO_REVIEWS_AND_COMMENTS" -> (SearchTypes.Query_histo_reviews_and_comments : SearchTypes.query_request_query_type)
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
    | "change_id" -> 
      let json = Js.Dict.unsafeGet json "change_id" in
      v.change_id <- Pbrt_bs.string json "query_request" "change_id"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    SearchTypes.index = v.index;
    SearchTypes.username = v.username;
    SearchTypes.query = v.query;
    SearchTypes.query_type = v.query_type;
    SearchTypes.order = v.order;
    SearchTypes.limit = v.limit;
    SearchTypes.change_id = v.change_id;
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
    | "changed_files_count" -> 
      let json = Js.Dict.unsafeGet json "changed_files_count" in
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
        (decode_task_data (Pbrt_bs.object_ json "change" "task_data"))
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

let rec decode_ratio json =
  let v = default_ratio_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "ratio" -> 
      let json = Js.Dict.unsafeGet json "ratio" in
      v.ratio <- Pbrt_bs.float json "ratio" "ratio"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    SearchTypes.ratio = v.ratio;
  } : SearchTypes.ratio)

let rec decode_change_event json =
  let v = default_change_event_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "id" -> 
      let json = Js.Dict.unsafeGet json "id" in
      v.id <- Pbrt_bs.string json "change_event" "id"
    | "type" -> 
      let json = Js.Dict.unsafeGet json "type" in
      v.type_ <- Pbrt_bs.string json "change_event" "type_"
    | "change_id" -> 
      let json = Js.Dict.unsafeGet json "change_id" in
      v.change_id <- Pbrt_bs.string json "change_event" "change_id"
    | "created_at" -> 
      let json = Js.Dict.unsafeGet json "created_at" in
      v.created_at <- Some ((TimestampBs.decode_timestamp (Pbrt_bs.string json "change_event" "created_at")))
    | "on_created_at" -> 
      let json = Js.Dict.unsafeGet json "on_created_at" in
      v.on_created_at <- Some ((TimestampBs.decode_timestamp (Pbrt_bs.string json "change_event" "on_created_at")))
    | "author" -> 
      let json = Js.Dict.unsafeGet json "author" in
      v.author <- Pbrt_bs.string json "change_event" "author"
    | "on_author" -> 
      let json = Js.Dict.unsafeGet json "on_author" in
      v.on_author <- Pbrt_bs.string json "change_event" "on_author"
    | "branch" -> 
      let json = Js.Dict.unsafeGet json "branch" in
      v.branch <- Pbrt_bs.string json "change_event" "branch"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    SearchTypes.id = v.id;
    SearchTypes.type_ = v.type_;
    SearchTypes.change_id = v.change_id;
    SearchTypes.created_at = v.created_at;
    SearchTypes.on_created_at = v.on_created_at;
    SearchTypes.author = v.author;
    SearchTypes.on_author = v.on_author;
    SearchTypes.branch = v.branch;
  } : SearchTypes.change_event)

let rec decode_change_and_events json =
  let v = default_change_and_events_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "change" -> 
      let json = Js.Dict.unsafeGet json "change" in
      v.change <- Some ((decode_change (Pbrt_bs.object_ json "change_and_events" "change")))
    | "events" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "events" in 
        Pbrt_bs.array_ a "change_and_events" "events"
      in
      v.events <- Array.map (fun json -> 
        (decode_change_event (Pbrt_bs.object_ json "change_and_events" "events"))
      ) a |> Array.to_list;
    end
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    SearchTypes.change = v.change;
    SearchTypes.events = v.events;
  } : SearchTypes.change_and_events)

let rec decode_review_count json =
  let v = default_review_count_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "authors_count" -> 
      let json = Js.Dict.unsafeGet json "authors_count" in
      v.authors_count <- Pbrt_bs.int32 json "review_count" "authors_count"
    | "events_count" -> 
      let json = Js.Dict.unsafeGet json "events_count" in
      v.events_count <- Pbrt_bs.int32 json "review_count" "events_count"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    SearchTypes.authors_count = v.authors_count;
    SearchTypes.events_count = v.events_count;
  } : SearchTypes.review_count)

let rec decode_histo json =
  let v = default_histo_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "date" -> 
      let json = Js.Dict.unsafeGet json "date" in
      v.date <- Pbrt_bs.string json "histo" "date"
    | "count" -> 
      let json = Js.Dict.unsafeGet json "count" in
      v.count <- Pbrt_bs.int32 json "histo" "count"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    SearchTypes.date = v.date;
    SearchTypes.count = v.count;
  } : SearchTypes.histo)

let rec decode_histo_stat json =
  let v = default_histo_stat_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "histo" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "histo" in 
        Pbrt_bs.array_ a "histo_stat" "histo"
      in
      v.histo <- Array.map (fun json -> 
        (decode_histo (Pbrt_bs.object_ json "histo_stat" "histo"))
      ) a |> Array.to_list;
    end
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    SearchTypes.histo = v.histo;
  } : SearchTypes.histo_stat)

let rec decode_review_stats json =
  let v = default_review_stats_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "comment_count" -> 
      let json = Js.Dict.unsafeGet json "comment_count" in
      v.comment_count <- Some ((decode_review_count (Pbrt_bs.object_ json "review_stats" "comment_count")))
    | "review_count" -> 
      let json = Js.Dict.unsafeGet json "review_count" in
      v.review_count <- Some ((decode_review_count (Pbrt_bs.object_ json "review_stats" "review_count")))
    | "comment_delay" -> 
      let json = Js.Dict.unsafeGet json "comment_delay" in
      v.comment_delay <- Pbrt_bs.int32 json "review_stats" "comment_delay"
    | "review_delay" -> 
      let json = Js.Dict.unsafeGet json "review_delay" in
      v.review_delay <- Pbrt_bs.int32 json "review_stats" "review_delay"
    | "comment_histo" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "comment_histo" in 
        Pbrt_bs.array_ a "review_stats" "comment_histo"
      in
      v.comment_histo <- Array.map (fun json -> 
        (decode_histo (Pbrt_bs.object_ json "review_stats" "comment_histo"))
      ) a |> Array.to_list;
    end
    | "review_histo" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "review_histo" in 
        Pbrt_bs.array_ a "review_stats" "review_histo"
      in
      v.review_histo <- Array.map (fun json -> 
        (decode_histo (Pbrt_bs.object_ json "review_stats" "review_histo"))
      ) a |> Array.to_list;
    end
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    SearchTypes.comment_count = v.comment_count;
    SearchTypes.review_count = v.review_count;
    SearchTypes.comment_delay = v.comment_delay;
    SearchTypes.review_delay = v.review_delay;
    SearchTypes.comment_histo = v.comment_histo;
    SearchTypes.review_histo = v.review_histo;
  } : SearchTypes.review_stats)

let rec decode_activity_stats json =
  let v = default_activity_stats_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "change_authors" -> 
      let json = Js.Dict.unsafeGet json "change_authors" in
      v.change_authors <- Pbrt_bs.int32 json "activity_stats" "change_authors"
    | "comment_authors" -> 
      let json = Js.Dict.unsafeGet json "comment_authors" in
      v.comment_authors <- Pbrt_bs.int32 json "activity_stats" "comment_authors"
    | "review_authors" -> 
      let json = Js.Dict.unsafeGet json "review_authors" in
      v.review_authors <- Pbrt_bs.int32 json "activity_stats" "review_authors"
    | "comments_histo" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "comments_histo" in 
        Pbrt_bs.array_ a "activity_stats" "comments_histo"
      in
      v.comments_histo <- Array.map (fun json -> 
        (decode_histo (Pbrt_bs.object_ json "activity_stats" "comments_histo"))
      ) a |> Array.to_list;
    end
    | "reviews_histo" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "reviews_histo" in 
        Pbrt_bs.array_ a "activity_stats" "reviews_histo"
      in
      v.reviews_histo <- Array.map (fun json -> 
        (decode_histo (Pbrt_bs.object_ json "activity_stats" "reviews_histo"))
      ) a |> Array.to_list;
    end
    | "changes_histo" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "changes_histo" in 
        Pbrt_bs.array_ a "activity_stats" "changes_histo"
      in
      v.changes_histo <- Array.map (fun json -> 
        (decode_histo (Pbrt_bs.object_ json "activity_stats" "changes_histo"))
      ) a |> Array.to_list;
    end
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    SearchTypes.change_authors = v.change_authors;
    SearchTypes.comment_authors = v.comment_authors;
    SearchTypes.review_authors = v.review_authors;
    SearchTypes.comments_histo = v.comments_histo;
    SearchTypes.reviews_histo = v.reviews_histo;
    SearchTypes.changes_histo = v.changes_histo;
  } : SearchTypes.activity_stats)

let rec decode_repo_summary json =
  let v = default_repo_summary_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "fullname" -> 
      let json = Js.Dict.unsafeGet json "fullname" in
      v.fullname <- Pbrt_bs.string json "repo_summary" "fullname"
    | "created_changes" -> 
      let json = Js.Dict.unsafeGet json "created_changes" in
      v.created_changes <- Pbrt_bs.int32 json "repo_summary" "created_changes"
    | "abandoned_changes" -> 
      let json = Js.Dict.unsafeGet json "abandoned_changes" in
      v.abandoned_changes <- Pbrt_bs.int32 json "repo_summary" "abandoned_changes"
    | "merged_changes" -> 
      let json = Js.Dict.unsafeGet json "merged_changes" in
      v.merged_changes <- Pbrt_bs.int32 json "repo_summary" "merged_changes"
    | "updated_changes" -> 
      let json = Js.Dict.unsafeGet json "updated_changes" in
      v.updated_changes <- Pbrt_bs.int32 json "repo_summary" "updated_changes"
    | "open_changes" -> 
      let json = Js.Dict.unsafeGet json "open_changes" in
      v.open_changes <- Pbrt_bs.int32 json "repo_summary" "open_changes"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    SearchTypes.fullname = v.fullname;
    SearchTypes.created_changes = v.created_changes;
    SearchTypes.abandoned_changes = v.abandoned_changes;
    SearchTypes.merged_changes = v.merged_changes;
    SearchTypes.updated_changes = v.updated_changes;
    SearchTypes.open_changes = v.open_changes;
  } : SearchTypes.repo_summary)

let rec decode_repos_summary json =
  let v = default_repos_summary_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "reposum" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "reposum" in 
        Pbrt_bs.array_ a "repos_summary" "reposum"
      in
      v.reposum <- Array.map (fun json -> 
        (decode_repo_summary (Pbrt_bs.object_ json "repos_summary" "reposum"))
      ) a |> Array.to_list;
    end
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    SearchTypes.reposum = v.reposum;
  } : SearchTypes.repos_summary)

let rec decode_term_count json =
  let v = default_term_count_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "term" -> 
      let json = Js.Dict.unsafeGet json "term" in
      v.term <- Pbrt_bs.string json "term_count" "term"
    | "count" -> 
      let json = Js.Dict.unsafeGet json "count" in
      v.count <- Pbrt_bs.int32 json "term_count" "count"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    SearchTypes.term = v.term;
    SearchTypes.count = v.count;
  } : SearchTypes.term_count)

let rec decode_terms_count json =
  let v = default_terms_count_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "termcount" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "termcount" in 
        Pbrt_bs.array_ a "terms_count" "termcount"
      in
      v.termcount <- Array.map (fun json -> 
        (decode_term_count (Pbrt_bs.object_ json "terms_count" "termcount"))
      ) a |> Array.to_list;
    end
    | "total_hits" -> 
      let json = Js.Dict.unsafeGet json "total_hits" in
      v.total_hits <- Pbrt_bs.int32 json "terms_count" "total_hits"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    SearchTypes.termcount = v.termcount;
    SearchTypes.total_hits = v.total_hits;
  } : SearchTypes.terms_count)

let rec decode_author_peer json =
  let v = default_author_peer_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "author" -> 
      let json = Js.Dict.unsafeGet json "author" in
      v.author <- Pbrt_bs.string json "author_peer" "author"
    | "peer" -> 
      let json = Js.Dict.unsafeGet json "peer" in
      v.peer <- Pbrt_bs.string json "author_peer" "peer"
    | "strength" -> 
      let json = Js.Dict.unsafeGet json "strength" in
      v.strength <- Pbrt_bs.int32 json "author_peer" "strength"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    SearchTypes.author = v.author;
    SearchTypes.peer = v.peer;
    SearchTypes.strength = v.strength;
  } : SearchTypes.author_peer)

let rec decode_authors_peers json =
  let v = default_authors_peers_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "author_peer" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "author_peer" in 
        Pbrt_bs.array_ a "authors_peers" "author_peer"
      in
      v.author_peer <- Array.map (fun json -> 
        (decode_author_peer (Pbrt_bs.object_ json "authors_peers" "author_peer"))
      ) a |> Array.to_list;
    end
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    SearchTypes.author_peer = v.author_peer;
  } : SearchTypes.authors_peers)

let rec decode_lifecycle_stats json =
  let v = default_lifecycle_stats_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "created_histo" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "created_histo" in 
        Pbrt_bs.array_ a "lifecycle_stats" "created_histo"
      in
      v.created_histo <- Array.map (fun json -> 
        (decode_histo (Pbrt_bs.object_ json "lifecycle_stats" "created_histo"))
      ) a |> Array.to_list;
    end
    | "updated_histo" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "updated_histo" in 
        Pbrt_bs.array_ a "lifecycle_stats" "updated_histo"
      in
      v.updated_histo <- Array.map (fun json -> 
        (decode_histo (Pbrt_bs.object_ json "lifecycle_stats" "updated_histo"))
      ) a |> Array.to_list;
    end
    | "merged_histo" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "merged_histo" in 
        Pbrt_bs.array_ a "lifecycle_stats" "merged_histo"
      in
      v.merged_histo <- Array.map (fun json -> 
        (decode_histo (Pbrt_bs.object_ json "lifecycle_stats" "merged_histo"))
      ) a |> Array.to_list;
    end
    | "abandoned_histo" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "abandoned_histo" in 
        Pbrt_bs.array_ a "lifecycle_stats" "abandoned_histo"
      in
      v.abandoned_histo <- Array.map (fun json -> 
        (decode_histo (Pbrt_bs.object_ json "lifecycle_stats" "abandoned_histo"))
      ) a |> Array.to_list;
    end
    | "created" -> 
      let json = Js.Dict.unsafeGet json "created" in
      v.created <- Some ((decode_review_count (Pbrt_bs.object_ json "lifecycle_stats" "created")))
    | "abandoned" -> 
      let json = Js.Dict.unsafeGet json "abandoned" in
      v.abandoned <- Pbrt_bs.int32 json "lifecycle_stats" "abandoned"
    | "merged" -> 
      let json = Js.Dict.unsafeGet json "merged" in
      v.merged <- Pbrt_bs.int32 json "lifecycle_stats" "merged"
    | "self_merged" -> 
      let json = Js.Dict.unsafeGet json "self_merged" in
      v.self_merged <- Pbrt_bs.int32 json "lifecycle_stats" "self_merged"
    | "self_mergedRatio" -> 
      let json = Js.Dict.unsafeGet json "self_mergedRatio" in
      v.self_merged_ratio <- Pbrt_bs.float json "lifecycle_stats" "self_merged_ratio"
    | "ttm_mean" -> 
      let json = Js.Dict.unsafeGet json "ttm_mean" in
      v.ttm_mean <- Pbrt_bs.float json "lifecycle_stats" "ttm_mean"
    | "ttm_variability" -> 
      let json = Js.Dict.unsafeGet json "ttm_variability" in
      v.ttm_variability <- Pbrt_bs.float json "lifecycle_stats" "ttm_variability"
    | "updates_of_changes" -> 
      let json = Js.Dict.unsafeGet json "updates_of_changes" in
      v.updates_of_changes <- Pbrt_bs.int32 json "lifecycle_stats" "updates_of_changes"
    | "changes_with_tests" -> 
      let json = Js.Dict.unsafeGet json "changes_with_tests" in
      v.changes_with_tests <- Pbrt_bs.float json "lifecycle_stats" "changes_with_tests"
    | "iterations_per_change" -> 
      let json = Js.Dict.unsafeGet json "iterations_per_change" in
      v.iterations_per_change <- Pbrt_bs.float json "lifecycle_stats" "iterations_per_change"
    | "commits_per_change" -> 
      let json = Js.Dict.unsafeGet json "commits_per_change" in
      v.commits_per_change <- Pbrt_bs.float json "lifecycle_stats" "commits_per_change"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    SearchTypes.created_histo = v.created_histo;
    SearchTypes.updated_histo = v.updated_histo;
    SearchTypes.merged_histo = v.merged_histo;
    SearchTypes.abandoned_histo = v.abandoned_histo;
    SearchTypes.created = v.created;
    SearchTypes.abandoned = v.abandoned;
    SearchTypes.merged = v.merged;
    SearchTypes.self_merged = v.self_merged;
    SearchTypes.self_merged_ratio = v.self_merged_ratio;
    SearchTypes.ttm_mean = v.ttm_mean;
    SearchTypes.ttm_variability = v.ttm_variability;
    SearchTypes.updates_of_changes = v.updates_of_changes;
    SearchTypes.changes_with_tests = v.changes_with_tests;
    SearchTypes.iterations_per_change = v.iterations_per_change;
    SearchTypes.commits_per_change = v.commits_per_change;
  } : SearchTypes.lifecycle_stats)

let rec decode_changes_tops json =
  let v = default_changes_tops_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "authors" -> 
      let json = Js.Dict.unsafeGet json "authors" in
      v.authors <- Some ((decode_terms_count (Pbrt_bs.object_ json "changes_tops" "authors")))
    | "repos" -> 
      let json = Js.Dict.unsafeGet json "repos" in
      v.repos <- Some ((decode_terms_count (Pbrt_bs.object_ json "changes_tops" "repos")))
    | "approvals" -> 
      let json = Js.Dict.unsafeGet json "approvals" in
      v.approvals <- Some ((decode_terms_count (Pbrt_bs.object_ json "changes_tops" "approvals")))
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    SearchTypes.authors = v.authors;
    SearchTypes.repos = v.repos;
    SearchTypes.approvals = v.approvals;
  } : SearchTypes.changes_tops)

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
      | "top_authors" -> 
        let json = Js.Dict.unsafeGet json "top_authors" in
        (SearchTypes.Top_authors ((decode_terms_count (Pbrt_bs.object_ json "query_response" "Top_authors"))) : SearchTypes.query_response)
      | "authors_peers" -> 
        let json = Js.Dict.unsafeGet json "authors_peers" in
        (SearchTypes.Authors_peers ((decode_authors_peers (Pbrt_bs.object_ json "query_response" "Authors_peers"))) : SearchTypes.query_response)
      | "new_authors" -> 
        let json = Js.Dict.unsafeGet json "new_authors" in
        (SearchTypes.New_authors ((decode_terms_count (Pbrt_bs.object_ json "query_response" "New_authors"))) : SearchTypes.query_response)
      | "review_stats" -> 
        let json = Js.Dict.unsafeGet json "review_stats" in
        (SearchTypes.Review_stats ((decode_review_stats (Pbrt_bs.object_ json "query_response" "Review_stats"))) : SearchTypes.query_response)
      | "lifecycle_stats" -> 
        let json = Js.Dict.unsafeGet json "lifecycle_stats" in
        (SearchTypes.Lifecycle_stats ((decode_lifecycle_stats (Pbrt_bs.object_ json "query_response" "Lifecycle_stats"))) : SearchTypes.query_response)
      | "activity_stats" -> 
        let json = Js.Dict.unsafeGet json "activity_stats" in
        (SearchTypes.Activity_stats ((decode_activity_stats (Pbrt_bs.object_ json "query_response" "Activity_stats"))) : SearchTypes.query_response)
      | "change_events" -> 
        let json = Js.Dict.unsafeGet json "change_events" in
        (SearchTypes.Change_events ((decode_change_and_events (Pbrt_bs.object_ json "query_response" "Change_events"))) : SearchTypes.query_response)
      | "changes_tops" -> 
        let json = Js.Dict.unsafeGet json "changes_tops" in
        (SearchTypes.Changes_tops ((decode_changes_tops (Pbrt_bs.object_ json "query_response" "Changes_tops"))) : SearchTypes.query_response)
      | "ratio" -> 
        let json = Js.Dict.unsafeGet json "ratio" in
        (SearchTypes.Ratio (Pbrt_bs.float json "query_response" "Ratio") : SearchTypes.query_response)
      | "histo" -> 
        let json = Js.Dict.unsafeGet json "histo" in
        (SearchTypes.Histo ((decode_histo_stat (Pbrt_bs.object_ json "query_response" "Histo"))) : SearchTypes.query_response)
      
      | _ -> loop (i - 1)
      end
  in
  loop (Array.length keys - 1)

let rec encode_task_data (v:SearchTypes.task_data) = 
  let json = Js.Dict.empty () in
  begin match v.SearchTypes.updated_at with
  | None -> ()
  | Some v ->
    begin (* updated_at field *)
      let json' = TimestampBs.encode_timestamp v in
      Js.Dict.set json "updated_at" (Js.Json.string json');
    end;
  end;
  Js.Dict.set json "change_url" (Js.Json.string v.SearchTypes.change_url);
  let a = v.SearchTypes.ttype |> Array.of_list |> Array.map Js.Json.string in
  Js.Dict.set json "ttype" (Js.Json.array a);
  Js.Dict.set json "tid" (Js.Json.string v.SearchTypes.tid);
  Js.Dict.set json "url" (Js.Json.string v.SearchTypes.url);
  Js.Dict.set json "title" (Js.Json.string v.SearchTypes.title);
  Js.Dict.set json "severity" (Js.Json.string v.SearchTypes.severity);
  Js.Dict.set json "priority" (Js.Json.string v.SearchTypes.priority);
  Js.Dict.set json "score" (Js.Json.number (Int32.to_float v.SearchTypes.score));
  Js.Dict.set json "prefix" (Js.Json.string v.SearchTypes.prefix);
  json

let rec encode_suggestions_request (v:SearchTypes.suggestions_request) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "index" (Js.Json.string v.SearchTypes.index);
  json

let rec encode_suggestions_response (v:SearchTypes.suggestions_response) = 
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
  let a = v.SearchTypes.projects |> Array.of_list |> Array.map Js.Json.string in
  Js.Dict.set json "projects" (Js.Json.array a);
  let a = v.SearchTypes.groups |> Array.of_list |> Array.map Js.Json.string in
  Js.Dict.set json "groups" (Js.Json.array a);
  let a = v.SearchTypes.labels |> Array.of_list |> Array.map Js.Json.string in
  Js.Dict.set json "labels" (Js.Json.array a);
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

let rec encode_check_request (v:SearchTypes.check_request) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "index" (Js.Json.string v.SearchTypes.index);
  Js.Dict.set json "username" (Js.Json.string v.SearchTypes.username);
  Js.Dict.set json "query" (Js.Json.string v.SearchTypes.query);
  json

let rec encode_check_response (v:SearchTypes.check_response) = 
  let json = Js.Dict.empty () in
  begin match v with
  | SearchTypes.Success v ->
    Js.Dict.set json "success" (Js.Json.string v);
  | SearchTypes.Error v ->
    begin (* error field *)
      let json' = encode_query_error v in
      Js.Dict.set json "error" (Js.Json.object_ json');
    end;
  end;
  json

let rec encode_author (v:SearchTypes.author) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "muid" (Js.Json.string v.SearchTypes.muid);
  let a = v.SearchTypes.aliases |> Array.of_list |> Array.map Js.Json.string in
  Js.Dict.set json "aliases" (Js.Json.array a);
  let a = v.SearchTypes.groups |> Array.of_list |> Array.map Js.Json.string in
  Js.Dict.set json "groups" (Js.Json.array a);
  json

let rec encode_author_request (v:SearchTypes.author_request) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "index" (Js.Json.string v.SearchTypes.index);
  Js.Dict.set json "query" (Js.Json.string v.SearchTypes.query);
  json

let rec encode_author_response (v:SearchTypes.author_response) = 
  let json = Js.Dict.empty () in
  begin (* authors field *)
    let (authors':Js.Json.t) =
      v.SearchTypes.authors
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_author |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "authors" authors';
  end;
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
  | SearchTypes.Query_repos_summary -> "QUERY_REPOS_SUMMARY"
  | SearchTypes.Query_top_authors_changes_created -> "QUERY_TOP_AUTHORS_CHANGES_CREATED"
  | SearchTypes.Query_top_authors_changes_merged -> "QUERY_TOP_AUTHORS_CHANGES_MERGED"
  | SearchTypes.Query_top_authors_changes_reviewed -> "QUERY_TOP_AUTHORS_CHANGES_REVIEWED"
  | SearchTypes.Query_top_authors_changes_commented -> "QUERY_TOP_AUTHORS_CHANGES_COMMENTED"
  | SearchTypes.Query_top_reviewed_authors -> "QUERY_TOP_REVIEWED_AUTHORS"
  | SearchTypes.Query_top_commented_authors -> "QUERY_TOP_COMMENTED_AUTHORS"
  | SearchTypes.Query_top_authors_peers -> "QUERY_TOP_AUTHORS_PEERS"
  | SearchTypes.Query_new_changes_authors -> "QUERY_NEW_CHANGES_AUTHORS"
  | SearchTypes.Query_changes_review_stats -> "QUERY_CHANGES_REVIEW_STATS"
  | SearchTypes.Query_changes_lifecycle_stats -> "QUERY_CHANGES_LIFECYCLE_STATS"
  | SearchTypes.Query_active_authors_stats -> "QUERY_ACTIVE_AUTHORS_STATS"
  | SearchTypes.Query_change_and_events -> "QUERY_CHANGE_AND_EVENTS"
  | SearchTypes.Query_changes_tops -> "QUERY_CHANGES_TOPS"
  | SearchTypes.Query_ratio_commits_vs_reviews -> "QUERY_RATIO_COMMITS_VS_REVIEWS"
  | SearchTypes.Query_histo_commits -> "QUERY_HISTO_COMMITS"
  | SearchTypes.Query_histo_reviews_and_comments -> "QUERY_HISTO_REVIEWS_AND_COMMENTS"

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
  Js.Dict.set json "change_id" (Js.Json.string v.SearchTypes.change_id);
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
    begin (* authored_at field *)
      let json' = TimestampBs.encode_timestamp v in
      Js.Dict.set json "authored_at" (Js.Json.string json');
    end;
  end;
  Js.Dict.set json "committer" (Js.Json.string v.SearchTypes.committer);
  begin match v.SearchTypes.committed_at with
  | None -> ()
  | Some v ->
    begin (* committed_at field *)
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
    begin (* created_at field *)
      let json' = TimestampBs.encode_timestamp v in
      Js.Dict.set json "created_at" (Js.Json.string json');
    end;
  end;
  begin match v.SearchTypes.updated_at with
  | None -> ()
  | Some v ->
    begin (* updated_at field *)
      let json' = TimestampBs.encode_timestamp v in
      Js.Dict.set json "updated_at" (Js.Json.string json');
    end;
  end;
  begin match v.SearchTypes.merged_at with
  | None -> ()
  | Some v ->
    begin (* merged_at field *)
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
  Js.Dict.set json "changed_files_count" (Js.Json.number (Int32.to_float v.SearchTypes.changed_files_count));
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
        v |> encode_task_data |> Js.Json.object_
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

let rec encode_ratio (v:SearchTypes.ratio) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "ratio" (Js.Json.number v.SearchTypes.ratio);
  json

let rec encode_change_event (v:SearchTypes.change_event) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "id" (Js.Json.string v.SearchTypes.id);
  Js.Dict.set json "type" (Js.Json.string v.SearchTypes.type_);
  Js.Dict.set json "change_id" (Js.Json.string v.SearchTypes.change_id);
  begin match v.SearchTypes.created_at with
  | None -> ()
  | Some v ->
    begin (* created_at field *)
      let json' = TimestampBs.encode_timestamp v in
      Js.Dict.set json "created_at" (Js.Json.string json');
    end;
  end;
  begin match v.SearchTypes.on_created_at with
  | None -> ()
  | Some v ->
    begin (* onCreated_at field *)
      let json' = TimestampBs.encode_timestamp v in
      Js.Dict.set json "on_created_at" (Js.Json.string json');
    end;
  end;
  Js.Dict.set json "author" (Js.Json.string v.SearchTypes.author);
  Js.Dict.set json "on_author" (Js.Json.string v.SearchTypes.on_author);
  Js.Dict.set json "branch" (Js.Json.string v.SearchTypes.branch);
  json

let rec encode_change_and_events (v:SearchTypes.change_and_events) = 
  let json = Js.Dict.empty () in
  begin match v.SearchTypes.change with
  | None -> ()
  | Some v ->
    begin (* change field *)
      let json' = encode_change v in
      Js.Dict.set json "change" (Js.Json.object_ json');
    end;
  end;
  begin (* events field *)
    let (events':Js.Json.t) =
      v.SearchTypes.events
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_change_event |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "events" events';
  end;
  json

let rec encode_review_count (v:SearchTypes.review_count) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "authors_count" (Js.Json.number (Int32.to_float v.SearchTypes.authors_count));
  Js.Dict.set json "events_count" (Js.Json.number (Int32.to_float v.SearchTypes.events_count));
  json

let rec encode_histo (v:SearchTypes.histo) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "date" (Js.Json.string v.SearchTypes.date);
  Js.Dict.set json "count" (Js.Json.number (Int32.to_float v.SearchTypes.count));
  json

let rec encode_histo_stat (v:SearchTypes.histo_stat) = 
  let json = Js.Dict.empty () in
  begin (* histo field *)
    let (histo':Js.Json.t) =
      v.SearchTypes.histo
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_histo |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "histo" histo';
  end;
  json

let rec encode_review_stats (v:SearchTypes.review_stats) = 
  let json = Js.Dict.empty () in
  begin match v.SearchTypes.comment_count with
  | None -> ()
  | Some v ->
    begin (* comment_count field *)
      let json' = encode_review_count v in
      Js.Dict.set json "comment_count" (Js.Json.object_ json');
    end;
  end;
  begin match v.SearchTypes.review_count with
  | None -> ()
  | Some v ->
    begin (* review_count field *)
      let json' = encode_review_count v in
      Js.Dict.set json "review_count" (Js.Json.object_ json');
    end;
  end;
  Js.Dict.set json "comment_delay" (Js.Json.number (Int32.to_float v.SearchTypes.comment_delay));
  Js.Dict.set json "review_delay" (Js.Json.number (Int32.to_float v.SearchTypes.review_delay));
  begin (* commentHisto field *)
    let (comment_histo':Js.Json.t) =
      v.SearchTypes.comment_histo
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_histo |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "comment_histo" comment_histo';
  end;
  begin (* reviewHisto field *)
    let (review_histo':Js.Json.t) =
      v.SearchTypes.review_histo
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_histo |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "review_histo" review_histo';
  end;
  json

let rec encode_activity_stats (v:SearchTypes.activity_stats) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "change_authors" (Js.Json.number (Int32.to_float v.SearchTypes.change_authors));
  Js.Dict.set json "comment_authors" (Js.Json.number (Int32.to_float v.SearchTypes.comment_authors));
  Js.Dict.set json "review_authors" (Js.Json.number (Int32.to_float v.SearchTypes.review_authors));
  begin (* commentsHisto field *)
    let (comments_histo':Js.Json.t) =
      v.SearchTypes.comments_histo
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_histo |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "comments_histo" comments_histo';
  end;
  begin (* reviewsHisto field *)
    let (reviews_histo':Js.Json.t) =
      v.SearchTypes.reviews_histo
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_histo |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "reviews_histo" reviews_histo';
  end;
  begin (* changesHisto field *)
    let (changes_histo':Js.Json.t) =
      v.SearchTypes.changes_histo
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_histo |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "changes_histo" changes_histo';
  end;
  json

let rec encode_repo_summary (v:SearchTypes.repo_summary) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "fullname" (Js.Json.string v.SearchTypes.fullname);
  Js.Dict.set json "created_changes" (Js.Json.number (Int32.to_float v.SearchTypes.created_changes));
  Js.Dict.set json "abandoned_changes" (Js.Json.number (Int32.to_float v.SearchTypes.abandoned_changes));
  Js.Dict.set json "merged_changes" (Js.Json.number (Int32.to_float v.SearchTypes.merged_changes));
  Js.Dict.set json "updated_changes" (Js.Json.number (Int32.to_float v.SearchTypes.updated_changes));
  Js.Dict.set json "open_changes" (Js.Json.number (Int32.to_float v.SearchTypes.open_changes));
  json

let rec encode_repos_summary (v:SearchTypes.repos_summary) = 
  let json = Js.Dict.empty () in
  begin (* reposum field *)
    let (reposum':Js.Json.t) =
      v.SearchTypes.reposum
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_repo_summary |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "reposum" reposum';
  end;
  json

let rec encode_term_count (v:SearchTypes.term_count) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "term" (Js.Json.string v.SearchTypes.term);
  Js.Dict.set json "count" (Js.Json.number (Int32.to_float v.SearchTypes.count));
  json

let rec encode_terms_count (v:SearchTypes.terms_count) = 
  let json = Js.Dict.empty () in
  begin (* termcount field *)
    let (termcount':Js.Json.t) =
      v.SearchTypes.termcount
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_term_count |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "termcount" termcount';
  end;
  Js.Dict.set json "total_hits" (Js.Json.number (Int32.to_float v.SearchTypes.total_hits));
  json

let rec encode_author_peer (v:SearchTypes.author_peer) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "author" (Js.Json.string v.SearchTypes.author);
  Js.Dict.set json "peer" (Js.Json.string v.SearchTypes.peer);
  Js.Dict.set json "strength" (Js.Json.number (Int32.to_float v.SearchTypes.strength));
  json

let rec encode_authors_peers (v:SearchTypes.authors_peers) = 
  let json = Js.Dict.empty () in
  begin (* authorPeer field *)
    let (author_peer':Js.Json.t) =
      v.SearchTypes.author_peer
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_author_peer |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "author_peer" author_peer';
  end;
  json

let rec encode_lifecycle_stats (v:SearchTypes.lifecycle_stats) = 
  let json = Js.Dict.empty () in
  begin (* createdHisto field *)
    let (created_histo':Js.Json.t) =
      v.SearchTypes.created_histo
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_histo |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "created_histo" created_histo';
  end;
  begin (* updatedHisto field *)
    let (updated_histo':Js.Json.t) =
      v.SearchTypes.updated_histo
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_histo |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "updated_histo" updated_histo';
  end;
  begin (* mergedHisto field *)
    let (merged_histo':Js.Json.t) =
      v.SearchTypes.merged_histo
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_histo |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "merged_histo" merged_histo';
  end;
  begin (* abandonedHisto field *)
    let (abandoned_histo':Js.Json.t) =
      v.SearchTypes.abandoned_histo
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_histo |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "abandoned_histo" abandoned_histo';
  end;
  begin match v.SearchTypes.created with
  | None -> ()
  | Some v ->
    begin (* created field *)
      let json' = encode_review_count v in
      Js.Dict.set json "created" (Js.Json.object_ json');
    end;
  end;
  Js.Dict.set json "abandoned" (Js.Json.number (Int32.to_float v.SearchTypes.abandoned));
  Js.Dict.set json "merged" (Js.Json.number (Int32.to_float v.SearchTypes.merged));
  Js.Dict.set json "self_merged" (Js.Json.number (Int32.to_float v.SearchTypes.self_merged));
  Js.Dict.set json "self_mergedRatio" (Js.Json.number v.SearchTypes.self_merged_ratio);
  Js.Dict.set json "ttm_mean" (Js.Json.number v.SearchTypes.ttm_mean);
  Js.Dict.set json "ttm_variability" (Js.Json.number v.SearchTypes.ttm_variability);
  Js.Dict.set json "updates_of_changes" (Js.Json.number (Int32.to_float v.SearchTypes.updates_of_changes));
  Js.Dict.set json "changes_with_tests" (Js.Json.number v.SearchTypes.changes_with_tests);
  Js.Dict.set json "iterations_per_change" (Js.Json.number v.SearchTypes.iterations_per_change);
  Js.Dict.set json "commits_per_change" (Js.Json.number v.SearchTypes.commits_per_change);
  json

let rec encode_changes_tops (v:SearchTypes.changes_tops) = 
  let json = Js.Dict.empty () in
  begin match v.SearchTypes.authors with
  | None -> ()
  | Some v ->
    begin (* authors field *)
      let json' = encode_terms_count v in
      Js.Dict.set json "authors" (Js.Json.object_ json');
    end;
  end;
  begin match v.SearchTypes.repos with
  | None -> ()
  | Some v ->
    begin (* repos field *)
      let json' = encode_terms_count v in
      Js.Dict.set json "repos" (Js.Json.object_ json');
    end;
  end;
  begin match v.SearchTypes.approvals with
  | None -> ()
  | Some v ->
    begin (* approvals field *)
      let json' = encode_terms_count v in
      Js.Dict.set json "approvals" (Js.Json.object_ json');
    end;
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
  | SearchTypes.Top_authors v ->
    begin (* topAuthors field *)
      let json' = encode_terms_count v in
      Js.Dict.set json "top_authors" (Js.Json.object_ json');
    end;
  | SearchTypes.Authors_peers v ->
    begin (* authorsPeers field *)
      let json' = encode_authors_peers v in
      Js.Dict.set json "authors_peers" (Js.Json.object_ json');
    end;
  | SearchTypes.New_authors v ->
    begin (* newAuthors field *)
      let json' = encode_terms_count v in
      Js.Dict.set json "new_authors" (Js.Json.object_ json');
    end;
  | SearchTypes.Review_stats v ->
    begin (* reviewStats field *)
      let json' = encode_review_stats v in
      Js.Dict.set json "review_stats" (Js.Json.object_ json');
    end;
  | SearchTypes.Lifecycle_stats v ->
    begin (* lifecycleStats field *)
      let json' = encode_lifecycle_stats v in
      Js.Dict.set json "lifecycle_stats" (Js.Json.object_ json');
    end;
  | SearchTypes.Activity_stats v ->
    begin (* activityStats field *)
      let json' = encode_activity_stats v in
      Js.Dict.set json "activity_stats" (Js.Json.object_ json');
    end;
  | SearchTypes.Change_events v ->
    begin (* changeEvents field *)
      let json' = encode_change_and_events v in
      Js.Dict.set json "change_events" (Js.Json.object_ json');
    end;
  | SearchTypes.Changes_tops v ->
    begin (* changesTops field *)
      let json' = encode_changes_tops v in
      Js.Dict.set json "changes_tops" (Js.Json.object_ json');
    end;
  | SearchTypes.Ratio v ->
    Js.Dict.set json "ratio" (Js.Json.number v);
  | SearchTypes.Histo v ->
    begin (* histo field *)
      let json' = encode_histo_stat v in
      Js.Dict.set json "histo" (Js.Json.object_ json');
    end;
  end;
  json
