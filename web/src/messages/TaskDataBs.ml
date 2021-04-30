[@@@ocaml.warning "-27-30-39"]

type task_data_commit_request_mutable = {
  mutable index : string;
  mutable crawler : string;
  mutable apikey : string;
  mutable timestamp : string;
}

let default_task_data_commit_request_mutable () : task_data_commit_request_mutable = {
  index = "";
  crawler = "";
  apikey = "";
  timestamp = "";
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
      v.timestamp <- Pbrt_bs.string json "task_data_commit_request" "timestamp"
    
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
        (TaskDataTypes.Timestamp (Pbrt_bs.string json "task_data_commit_response" "Timestamp") : TaskDataTypes.task_data_commit_response)
      
      | _ -> loop (i - 1)
      end
  in
  loop (Array.length keys - 1)

let rec encode_task_data_commit_request (v:TaskDataTypes.task_data_commit_request) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "index" (Js.Json.string v.TaskDataTypes.index);
  Js.Dict.set json "crawler" (Js.Json.string v.TaskDataTypes.crawler);
  Js.Dict.set json "apikey" (Js.Json.string v.TaskDataTypes.apikey);
  Js.Dict.set json "timestamp" (Js.Json.string v.TaskDataTypes.timestamp);
  json

let rec encode_task_data_commit_error (v:TaskDataTypes.task_data_commit_error) : string = 
  match v with
  | TaskDataTypes.Unknown_index -> "UnknownIndex"
  | TaskDataTypes.Unknown_crawler -> "UnknownCrawler"
  | TaskDataTypes.Unknown_api_key -> "UnknownApiKey"
  | TaskDataTypes.Commit_date_inferior_than_previous -> "CommitDateInferiorThanPrevious"

let rec encode_task_data_commit_response (v:TaskDataTypes.task_data_commit_response) = 
  let json = Js.Dict.empty () in
  begin match v with
  | TaskDataTypes.Error v ->
    Js.Dict.set json "error" (Js.Json.string (encode_task_data_commit_error v));
  | TaskDataTypes.Timestamp v ->
    Js.Dict.set json "timestamp" (Js.Json.string v);
  end;
  json
