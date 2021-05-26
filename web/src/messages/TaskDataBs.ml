[@@@ocaml.warning "-27-30-39"]

type task_data_commit_request_mutable = {
  mutable index : string;
  mutable crawler : string;
  mutable apikey : string;
  mutable timestamp : TimestampTypes.timestamp option;
}

let default_task_data_commit_request_mutable () : task_data_commit_request_mutable = {
  index = "";
  crawler = "";
  apikey = "";
  timestamp = None;
}

type task_data_get_last_updated_request_mutable = {
  mutable index : string;
  mutable crawler : string;
}

let default_task_data_get_last_updated_request_mutable () : task_data_get_last_updated_request_mutable = {
  index = "";
  crawler = "";
}

type new_task_data_mutable = {
  mutable updated_at : TimestampTypes.timestamp option;
  mutable change_url : string;
  mutable ttype : string list;
  mutable tid : string;
  mutable url : string;
  mutable title : string;
  mutable severity : string;
  mutable priority : string;
  mutable score : int32;
}

let default_new_task_data_mutable () : new_task_data_mutable = {
  updated_at = None;
  change_url = "";
  ttype = [];
  tid = "";
  url = "";
  title = "";
  severity = "";
  priority = "";
  score = 0l;
}

type add_request_mutable = {
  mutable index : string;
  mutable crawler : string;
  mutable apikey : string;
  mutable items : TaskDataTypes.new_task_data list;
}

let default_add_request_mutable () : add_request_mutable = {
  index = "";
  crawler = "";
  apikey = "";
  items = [];
}


let rec decode_task_data_commit_request json =
  let v = default_task_data_commit_request_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "index" -> 
      let json = Js.Dict.unsafeGet json "index" in
      v.index <- Pbrt_bs.string json "task_data_commit_request" "index"
    | "crawler" -> 
      let json = Js.Dict.unsafeGet json "crawler" in
      v.crawler <- Pbrt_bs.string json "task_data_commit_request" "crawler"
    | "apikey" -> 
      let json = Js.Dict.unsafeGet json "apikey" in
      v.apikey <- Pbrt_bs.string json "task_data_commit_request" "apikey"
    | "timestamp" -> 
      let json = Js.Dict.unsafeGet json "timestamp" in
      v.timestamp <- Some ((TimestampBs.decode_timestamp (Pbrt_bs.string json "task_data_commit_request" "timestamp")))
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    TaskDataTypes.index = v.index;
    TaskDataTypes.crawler = v.crawler;
    TaskDataTypes.apikey = v.apikey;
    TaskDataTypes.timestamp = v.timestamp;
  } : TaskDataTypes.task_data_commit_request)

let rec decode_task_data_commit_error (json:Js.Json.t) =
  match Pbrt_bs.string json "task_data_commit_error" "value" with
  | "UnknownIndex" -> (TaskDataTypes.Unknown_index : TaskDataTypes.task_data_commit_error)
  | "UnknownCrawler" -> (TaskDataTypes.Unknown_crawler : TaskDataTypes.task_data_commit_error)
  | "UnknownApiKey" -> (TaskDataTypes.Unknown_api_key : TaskDataTypes.task_data_commit_error)
  | "CommitDateInferiorThanPrevious" -> (TaskDataTypes.Commit_date_inferior_than_previous : TaskDataTypes.task_data_commit_error)
  | "AddFailed" -> (TaskDataTypes.Add_failed : TaskDataTypes.task_data_commit_error)
  | "" -> TaskDataTypes.Unknown_index
  | _ -> Pbrt_bs.E.malformed_variant "task_data_commit_error"

let rec decode_task_data_commit_response json =
  let keys = Js.Dict.keys json in
  let rec loop = function 
    | -1 -> Pbrt_bs.E.malformed_variant "task_data_commit_response"
    | i -> 
      begin match Array.unsafe_get keys i with
      | "error" -> 
        let json = Js.Dict.unsafeGet json "error" in
        (TaskDataTypes.Error ((decode_task_data_commit_error json)) : TaskDataTypes.task_data_commit_response)
      | "timestamp" -> 
        let json = Js.Dict.unsafeGet json "timestamp" in
        (TaskDataTypes.Timestamp ((TimestampBs.decode_timestamp (Pbrt_bs.string json "task_data_commit_response" "Timestamp"))) : TaskDataTypes.task_data_commit_response)
      
      | _ -> loop (i - 1)
      end
  in
  loop (Array.length keys - 1)

let rec decode_task_data_get_last_updated_error (json:Js.Json.t) =
  match Pbrt_bs.string json "task_data_get_last_updated_error" "value" with
  | "GetUnknownIndex" -> (TaskDataTypes.Get_unknown_index : TaskDataTypes.task_data_get_last_updated_error)
  | "GetUnknownCrawler" -> (TaskDataTypes.Get_unknown_crawler : TaskDataTypes.task_data_get_last_updated_error)
  | "" -> TaskDataTypes.Get_unknown_index
  | _ -> Pbrt_bs.E.malformed_variant "task_data_get_last_updated_error"

let rec decode_task_data_get_last_updated_request json =
  let v = default_task_data_get_last_updated_request_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "index" -> 
      let json = Js.Dict.unsafeGet json "index" in
      v.index <- Pbrt_bs.string json "task_data_get_last_updated_request" "index"
    | "crawler" -> 
      let json = Js.Dict.unsafeGet json "crawler" in
      v.crawler <- Pbrt_bs.string json "task_data_get_last_updated_request" "crawler"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    TaskDataTypes.index = v.index;
    TaskDataTypes.crawler = v.crawler;
  } : TaskDataTypes.task_data_get_last_updated_request)

let rec decode_task_data_get_last_updated_response json =
  let keys = Js.Dict.keys json in
  let rec loop = function 
    | -1 -> Pbrt_bs.E.malformed_variant "task_data_get_last_updated_response"
    | i -> 
      begin match Array.unsafe_get keys i with
      | "error" -> 
        let json = Js.Dict.unsafeGet json "error" in
        (TaskDataTypes.Error ((decode_task_data_get_last_updated_error json)) : TaskDataTypes.task_data_get_last_updated_response)
      | "timestamp" -> 
        let json = Js.Dict.unsafeGet json "timestamp" in
        (TaskDataTypes.Timestamp ((TimestampBs.decode_timestamp (Pbrt_bs.string json "task_data_get_last_updated_response" "Timestamp"))) : TaskDataTypes.task_data_get_last_updated_response)
      
      | _ -> loop (i - 1)
      end
  in
  loop (Array.length keys - 1)

let rec decode_new_task_data json =
  let v = default_new_task_data_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "updated_at" -> 
      let json = Js.Dict.unsafeGet json "updated_at" in
      v.updated_at <- Some ((TimestampBs.decode_timestamp (Pbrt_bs.string json "new_task_data" "updated_at")))
    | "change_url" -> 
      let json = Js.Dict.unsafeGet json "change_url" in
      v.change_url <- Pbrt_bs.string json "new_task_data" "change_url"
    | "ttype" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "ttype" in 
        Pbrt_bs.array_ a "new_task_data" "ttype"
      in
      v.ttype <- Array.map (fun json -> 
        Pbrt_bs.string json "new_task_data" "ttype"
      ) a |> Array.to_list;
    end
    | "tid" -> 
      let json = Js.Dict.unsafeGet json "tid" in
      v.tid <- Pbrt_bs.string json "new_task_data" "tid"
    | "url" -> 
      let json = Js.Dict.unsafeGet json "url" in
      v.url <- Pbrt_bs.string json "new_task_data" "url"
    | "title" -> 
      let json = Js.Dict.unsafeGet json "title" in
      v.title <- Pbrt_bs.string json "new_task_data" "title"
    | "severity" -> 
      let json = Js.Dict.unsafeGet json "severity" in
      v.severity <- Pbrt_bs.string json "new_task_data" "severity"
    | "priority" -> 
      let json = Js.Dict.unsafeGet json "priority" in
      v.priority <- Pbrt_bs.string json "new_task_data" "priority"
    | "score" -> 
      let json = Js.Dict.unsafeGet json "score" in
      v.score <- Pbrt_bs.int32 json "new_task_data" "score"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    TaskDataTypes.updated_at = v.updated_at;
    TaskDataTypes.change_url = v.change_url;
    TaskDataTypes.ttype = v.ttype;
    TaskDataTypes.tid = v.tid;
    TaskDataTypes.url = v.url;
    TaskDataTypes.title = v.title;
    TaskDataTypes.severity = v.severity;
    TaskDataTypes.priority = v.priority;
    TaskDataTypes.score = v.score;
  } : TaskDataTypes.new_task_data)

let rec decode_add_request json =
  let v = default_add_request_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "index" -> 
      let json = Js.Dict.unsafeGet json "index" in
      v.index <- Pbrt_bs.string json "add_request" "index"
    | "crawler" -> 
      let json = Js.Dict.unsafeGet json "crawler" in
      v.crawler <- Pbrt_bs.string json "add_request" "crawler"
    | "apikey" -> 
      let json = Js.Dict.unsafeGet json "apikey" in
      v.apikey <- Pbrt_bs.string json "add_request" "apikey"
    | "items" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "items" in 
        Pbrt_bs.array_ a "add_request" "items"
      in
      v.items <- Array.map (fun json -> 
        (decode_new_task_data (Pbrt_bs.object_ json "add_request" "items"))
      ) a |> Array.to_list;
    end
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    TaskDataTypes.index = v.index;
    TaskDataTypes.crawler = v.crawler;
    TaskDataTypes.apikey = v.apikey;
    TaskDataTypes.items = v.items;
  } : TaskDataTypes.add_request)

let rec decode_add_response json =
  let keys = Js.Dict.keys json in
  let rec loop = function 
    | -1 -> Pbrt_bs.E.malformed_variant "add_response"
    | i -> 
      begin match Array.unsafe_get keys i with
      | "error" -> 
        let json = Js.Dict.unsafeGet json "error" in
        (TaskDataTypes.Error ((decode_task_data_commit_error json)) : TaskDataTypes.add_response)
      
      | _ -> loop (i - 1)
      end
  in
  loop (Array.length keys - 1)

let rec encode_task_data_commit_request (v:TaskDataTypes.task_data_commit_request) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "index" (Js.Json.string v.TaskDataTypes.index);
  Js.Dict.set json "crawler" (Js.Json.string v.TaskDataTypes.crawler);
  Js.Dict.set json "apikey" (Js.Json.string v.TaskDataTypes.apikey);
  begin match v.TaskDataTypes.timestamp with
  | None -> ()
  | Some v ->
    begin (* timestamp field *)
      let json' = TimestampBs.encode_timestamp v in
      Js.Dict.set json "timestamp" (Js.Json.string json');
    end;
  end;
  json

let rec encode_task_data_commit_error (v:TaskDataTypes.task_data_commit_error) : string = 
  match v with
  | TaskDataTypes.Unknown_index -> "UnknownIndex"
  | TaskDataTypes.Unknown_crawler -> "UnknownCrawler"
  | TaskDataTypes.Unknown_api_key -> "UnknownApiKey"
  | TaskDataTypes.Commit_date_inferior_than_previous -> "CommitDateInferiorThanPrevious"
  | TaskDataTypes.Add_failed -> "AddFailed"

let rec encode_task_data_commit_response (v:TaskDataTypes.task_data_commit_response) = 
  let json = Js.Dict.empty () in
  begin match v with
  | TaskDataTypes.Error v ->
    Js.Dict.set json "error" (Js.Json.string (encode_task_data_commit_error v));
  | TaskDataTypes.Timestamp v ->
    begin (* timestamp field *)
      let json' = TimestampBs.encode_timestamp v in
      Js.Dict.set json "timestamp" (Js.Json.string json');
    end;
  end;
  json

let rec encode_task_data_get_last_updated_error (v:TaskDataTypes.task_data_get_last_updated_error) : string = 
  match v with
  | TaskDataTypes.Get_unknown_index -> "GetUnknownIndex"
  | TaskDataTypes.Get_unknown_crawler -> "GetUnknownCrawler"

let rec encode_task_data_get_last_updated_request (v:TaskDataTypes.task_data_get_last_updated_request) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "index" (Js.Json.string v.TaskDataTypes.index);
  Js.Dict.set json "crawler" (Js.Json.string v.TaskDataTypes.crawler);
  json

let rec encode_task_data_get_last_updated_response (v:TaskDataTypes.task_data_get_last_updated_response) = 
  let json = Js.Dict.empty () in
  begin match v with
  | TaskDataTypes.Error v ->
    Js.Dict.set json "error" (Js.Json.string (encode_task_data_get_last_updated_error v));
  | TaskDataTypes.Timestamp v ->
    begin (* timestamp field *)
      let json' = TimestampBs.encode_timestamp v in
      Js.Dict.set json "timestamp" (Js.Json.string json');
    end;
  end;
  json

let rec encode_new_task_data (v:TaskDataTypes.new_task_data) = 
  let json = Js.Dict.empty () in
  begin match v.TaskDataTypes.updated_at with
  | None -> ()
  | Some v ->
    begin (* updatedAt field *)
      let json' = TimestampBs.encode_timestamp v in
      Js.Dict.set json "updated_at" (Js.Json.string json');
    end;
  end;
  Js.Dict.set json "change_url" (Js.Json.string v.TaskDataTypes.change_url);
  let a = v.TaskDataTypes.ttype |> Array.of_list |> Array.map Js.Json.string in
  Js.Dict.set json "ttype" (Js.Json.array a);
  Js.Dict.set json "tid" (Js.Json.string v.TaskDataTypes.tid);
  Js.Dict.set json "url" (Js.Json.string v.TaskDataTypes.url);
  Js.Dict.set json "title" (Js.Json.string v.TaskDataTypes.title);
  Js.Dict.set json "severity" (Js.Json.string v.TaskDataTypes.severity);
  Js.Dict.set json "priority" (Js.Json.string v.TaskDataTypes.priority);
  Js.Dict.set json "score" (Js.Json.number (Int32.to_float v.TaskDataTypes.score));
  json

let rec encode_add_request (v:TaskDataTypes.add_request) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "index" (Js.Json.string v.TaskDataTypes.index);
  Js.Dict.set json "crawler" (Js.Json.string v.TaskDataTypes.crawler);
  Js.Dict.set json "apikey" (Js.Json.string v.TaskDataTypes.apikey);
  begin (* items field *)
    let (items':Js.Json.t) =
      v.TaskDataTypes.items
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_new_task_data |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "items" items';
  end;
  json

let rec encode_add_response (v:TaskDataTypes.add_response) = 
  let json = Js.Dict.empty () in
  begin match v with
  | TaskDataTypes.Error v ->
    Js.Dict.set json "error" (Js.Json.string (encode_task_data_commit_error v));
  end;
  json
