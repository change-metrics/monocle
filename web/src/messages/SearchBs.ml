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

type changes_query_request_mutable = {
  mutable index : string;
  mutable query : string;
}

let default_changes_query_request_mutable () : changes_query_request_mutable = {
  index = "";
  query = "";
}

type change_mutable = {
  mutable change_id : string;
  mutable author : string;
  mutable title : string;
  mutable url : string;
  mutable repository_fullname : string;
  mutable state : string;
  mutable branch : string;
  mutable created_at : TimestampTypes.timestamp option;
  mutable task_data : TaskDataTypes.new_task_data list;
}

let default_change_mutable () : change_mutable = {
  change_id = "";
  author = "";
  title = "";
  url = "";
  repository_fullname = "";
  state = "";
  branch = "";
  created_at = None;
  task_data = [];
}

type changes_mutable = {
  mutable changes : SearchTypes.change list;
}

let default_changes_mutable () : changes_mutable = {
  changes = [];
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

let rec decode_changes_query_request json =
  let v = default_changes_query_request_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "index" -> 
      let json = Js.Dict.unsafeGet json "index" in
      v.index <- Pbrt_bs.string json "changes_query_request" "index"
    | "query" -> 
      let json = Js.Dict.unsafeGet json "query" in
      v.query <- Pbrt_bs.string json "changes_query_request" "query"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    SearchTypes.index = v.index;
    SearchTypes.query = v.query;
  } : SearchTypes.changes_query_request)

let rec decode_change json =
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
    | "created_at" -> 
      let json = Js.Dict.unsafeGet json "created_at" in
      v.created_at <- Some ((TimestampBs.decode_timestamp (Pbrt_bs.string json "change" "created_at")))
    | "task_data" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "task_data" in 
        Pbrt_bs.array_ a "change" "task_data"
      in
      v.task_data <- Array.map (fun json -> 
        (TaskDataBs.decode_new_task_data (Pbrt_bs.object_ json "change" "task_data"))
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
    SearchTypes.created_at = v.created_at;
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

let rec decode_changes_query_response json =
  let keys = Js.Dict.keys json in
  let rec loop = function 
    | -1 -> Pbrt_bs.E.malformed_variant "changes_query_response"
    | i -> 
      begin match Array.unsafe_get keys i with
      | "error" -> 
        let json = Js.Dict.unsafeGet json "error" in
        (SearchTypes.Error ((decode_query_error (Pbrt_bs.object_ json "changes_query_response" "Error"))) : SearchTypes.changes_query_response)
      | "items" -> 
        let json = Js.Dict.unsafeGet json "items" in
        (SearchTypes.Items ((decode_changes (Pbrt_bs.object_ json "changes_query_response" "Items"))) : SearchTypes.changes_query_response)
      
      | _ -> loop (i - 1)
      end
  in
  loop (Array.length keys - 1)

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

let rec encode_changes_query_request (v:SearchTypes.changes_query_request) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "index" (Js.Json.string v.SearchTypes.index);
  Js.Dict.set json "query" (Js.Json.string v.SearchTypes.query);
  json

let rec encode_change (v:SearchTypes.change) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "change_id" (Js.Json.string v.SearchTypes.change_id);
  Js.Dict.set json "author" (Js.Json.string v.SearchTypes.author);
  Js.Dict.set json "title" (Js.Json.string v.SearchTypes.title);
  Js.Dict.set json "url" (Js.Json.string v.SearchTypes.url);
  Js.Dict.set json "repository_fullname" (Js.Json.string v.SearchTypes.repository_fullname);
  Js.Dict.set json "state" (Js.Json.string v.SearchTypes.state);
  Js.Dict.set json "branch" (Js.Json.string v.SearchTypes.branch);
  begin match v.SearchTypes.created_at with
  | None -> ()
  | Some v ->
    begin (* createdAt field *)
      let json' = TimestampBs.encode_timestamp v in
      Js.Dict.set json "created_at" (Js.Json.string json');
    end;
  end;
  begin (* taskData field *)
    let (task_data':Js.Json.t) =
      v.SearchTypes.task_data
      |> Array.of_list
      |> Array.map (fun v ->
        v |> TaskDataBs.encode_new_task_data |> Js.Json.object_
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

let rec encode_changes_query_response (v:SearchTypes.changes_query_response) = 
  let json = Js.Dict.empty () in
  begin match v with
  | SearchTypes.Error v ->
    begin (* error field *)
      let json' = encode_query_error v in
      Js.Dict.set json "error" (Js.Json.object_ json');
    end;
  | SearchTypes.Items v ->
    begin (* items field *)
      let json' = encode_changes v in
      Js.Dict.set json "items" (Js.Json.object_ json');
    end;
  end;
  json
