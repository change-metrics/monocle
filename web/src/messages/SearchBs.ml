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
  mutable title : string;
  mutable url : string;
  mutable created_at : TimestampTypes.timestamp option;
}

let default_change_mutable () : change_mutable = {
  title = "";
  url = "";
  created_at = None;
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
    | "title" -> 
      let json = Js.Dict.unsafeGet json "title" in
      v.title <- Pbrt_bs.string json "change" "title"
    | "url" -> 
      let json = Js.Dict.unsafeGet json "url" in
      v.url <- Pbrt_bs.string json "change" "url"
    | "created_at" -> 
      let json = Js.Dict.unsafeGet json "created_at" in
      v.created_at <- Some ((TimestampBs.decode_timestamp (Pbrt_bs.string json "change" "created_at")))
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    SearchTypes.title = v.title;
    SearchTypes.url = v.url;
    SearchTypes.created_at = v.created_at;
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
  Js.Dict.set json "title" (Js.Json.string v.SearchTypes.title);
  Js.Dict.set json "url" (Js.Json.string v.SearchTypes.url);
  begin match v.SearchTypes.created_at with
  | None -> ()
  | Some v ->
    begin (* createdAt field *)
      let json' = TimestampBs.encode_timestamp v in
      Js.Dict.set json "created_at" (Js.Json.string json');
    end;
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
