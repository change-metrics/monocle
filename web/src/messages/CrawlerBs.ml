[@@@ocaml.warning "-27-30-39"]

type crawler_error_mutable = {
  mutable message : string;
  mutable body : string;
  mutable created_at : TimestampTypes.timestamp option;
}

let default_crawler_error_mutable () : crawler_error_mutable = {
  message = "";
  body = "";
  created_at = None;
}

type crawler_error_list_mutable = {
  mutable crawler : string;
  mutable entity : CrawlerTypes.entity option;
  mutable errors : CrawlerTypes.crawler_error list;
}

let default_crawler_error_list_mutable () : crawler_error_list_mutable = {
  crawler = "";
  entity = None;
  errors = [];
}

type errors_request_mutable = {
  mutable index : string;
  mutable query : string;
}

let default_errors_request_mutable () : errors_request_mutable = {
  index = "";
  query = "";
}

type errors_list_mutable = {
  mutable errors : CrawlerTypes.crawler_error_list list;
}

let default_errors_list_mutable () : errors_list_mutable = {
  errors = [];
}

type project_mutable = {
  mutable full_path : string;
}

let default_project_mutable () : project_mutable = {
  full_path = "";
}

type add_doc_request_mutable = {
  mutable index : string;
  mutable crawler : string;
  mutable apikey : string;
  mutable entity : CrawlerTypes.entity option;
  mutable changes : ChangeTypes.change list;
  mutable events : ChangeTypes.change_event list;
  mutable projects : CrawlerTypes.project list;
  mutable task_datas : SearchTypes.task_data list;
  mutable issues : IssueTypes.issue list;
  mutable issue_events : IssueTypes.issue_event list;
  mutable errors : CrawlerTypes.crawler_error list;
}

let default_add_doc_request_mutable () : add_doc_request_mutable = {
  index = "";
  crawler = "";
  apikey = "";
  entity = None;
  changes = [];
  events = [];
  projects = [];
  task_datas = [];
  issues = [];
  issue_events = [];
  errors = [];
}

type commit_request_mutable = {
  mutable index : string;
  mutable crawler : string;
  mutable apikey : string;
  mutable entity : CrawlerTypes.entity option;
  mutable timestamp : TimestampTypes.timestamp option;
}

let default_commit_request_mutable () : commit_request_mutable = {
  index = "";
  crawler = "";
  apikey = "";
  entity = None;
  timestamp = None;
}

type commit_info_request_mutable = {
  mutable index : string;
  mutable crawler : string;
  mutable entity : CrawlerTypes.entity_type;
  mutable offset : int32;
}

let default_commit_info_request_mutable () : commit_info_request_mutable = {
  index = "";
  crawler = "";
  entity = CrawlerTypes.default_entity_type ();
  offset = 0l;
}

type commit_info_response_oldest_entity_mutable = {
  mutable entity : CrawlerTypes.entity option;
  mutable last_commit_at : TimestampTypes.timestamp option;
}

let default_commit_info_response_oldest_entity_mutable () : commit_info_response_oldest_entity_mutable = {
  entity = None;
  last_commit_at = None;
}


let rec decode_entity json =
  let keys = Js.Dict.keys json in
  let rec loop = function 
    | -1 -> Pbrt_bs.E.malformed_variant "entity"
    | i -> 
      begin match Array.unsafe_get keys i with
      | "organization_name" -> 
        let json = Js.Dict.unsafeGet json "organization_name" in
        (CrawlerTypes.Organization_name (Pbrt_bs.string json "entity" "Organization_name") : CrawlerTypes.entity)
      | "project_name" -> 
        let json = Js.Dict.unsafeGet json "project_name" in
        (CrawlerTypes.Project_name (Pbrt_bs.string json "entity" "Project_name") : CrawlerTypes.entity)
      | "project_issueName" -> 
        let json = Js.Dict.unsafeGet json "project_issueName" in
        (CrawlerTypes.Project_issue_name (Pbrt_bs.string json "entity" "Project_issue_name") : CrawlerTypes.entity)
      | "td_name" -> 
        let json = Js.Dict.unsafeGet json "td_name" in
        (CrawlerTypes.Td_name (Pbrt_bs.string json "entity" "Td_name") : CrawlerTypes.entity)
      | "user_name" -> 
        let json = Js.Dict.unsafeGet json "user_name" in
        (CrawlerTypes.User_name (Pbrt_bs.string json "entity" "User_name") : CrawlerTypes.entity)
      
      | _ -> loop (i - 1)
      end
  in
  loop (Array.length keys - 1)

let rec decode_entity_type (json:Js.Json.t) =
  match Pbrt_bs.string json "entity_type" "value" with
  | "ENTITY_TYPE_ORGANIZATION" -> (CrawlerTypes.Entity_type_organization : CrawlerTypes.entity_type)
  | "ENTITY_TYPE_PROJECT" -> (CrawlerTypes.Entity_type_project : CrawlerTypes.entity_type)
  | "ENTITY_TYPE_TASK_DATA" -> (CrawlerTypes.Entity_type_task_data : CrawlerTypes.entity_type)
  | "ENTITY_TYPE_USER" -> (CrawlerTypes.Entity_type_user : CrawlerTypes.entity_type)
  | "" -> CrawlerTypes.Entity_type_organization
  | _ -> Pbrt_bs.E.malformed_variant "entity_type"

let rec decode_crawler_error json =
  let v = default_crawler_error_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "message" -> 
      let json = Js.Dict.unsafeGet json "message" in
      v.message <- Pbrt_bs.string json "crawler_error" "message"
    | "body" -> 
      let json = Js.Dict.unsafeGet json "body" in
      v.body <- Pbrt_bs.string json "crawler_error" "body"
    | "created_at" -> 
      let json = Js.Dict.unsafeGet json "created_at" in
      v.created_at <- Some ((TimestampBs.decode_timestamp (Pbrt_bs.string json "crawler_error" "created_at")))
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    CrawlerTypes.message = v.message;
    CrawlerTypes.body = v.body;
    CrawlerTypes.created_at = v.created_at;
  } : CrawlerTypes.crawler_error)

let rec decode_crawler_error_list json =
  let v = default_crawler_error_list_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "crawler" -> 
      let json = Js.Dict.unsafeGet json "crawler" in
      v.crawler <- Pbrt_bs.string json "crawler_error_list" "crawler"
    | "entity" -> 
      let json = Js.Dict.unsafeGet json "entity" in
      v.entity <- Some ((decode_entity (Pbrt_bs.object_ json "crawler_error_list" "entity")))
    | "errors" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "errors" in 
        Pbrt_bs.array_ a "crawler_error_list" "errors"
      in
      v.errors <- Array.map (fun json -> 
        (decode_crawler_error (Pbrt_bs.object_ json "crawler_error_list" "errors"))
      ) a |> Array.to_list;
    end
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    CrawlerTypes.crawler = v.crawler;
    CrawlerTypes.entity = v.entity;
    CrawlerTypes.errors = v.errors;
  } : CrawlerTypes.crawler_error_list)

let rec decode_errors_request json =
  let v = default_errors_request_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "index" -> 
      let json = Js.Dict.unsafeGet json "index" in
      v.index <- Pbrt_bs.string json "errors_request" "index"
    | "query" -> 
      let json = Js.Dict.unsafeGet json "query" in
      v.query <- Pbrt_bs.string json "errors_request" "query"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    CrawlerTypes.index = v.index;
    CrawlerTypes.query = v.query;
  } : CrawlerTypes.errors_request)

let rec decode_errors_list json =
  let v = default_errors_list_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "errors" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "errors" in 
        Pbrt_bs.array_ a "errors_list" "errors"
      in
      v.errors <- Array.map (fun json -> 
        (decode_crawler_error_list (Pbrt_bs.object_ json "errors_list" "errors"))
      ) a |> Array.to_list;
    end
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    CrawlerTypes.errors = v.errors;
  } : CrawlerTypes.errors_list)

let rec decode_errors_response json =
  let keys = Js.Dict.keys json in
  let rec loop = function 
    | -1 -> Pbrt_bs.E.malformed_variant "errors_response"
    | i -> 
      begin match Array.unsafe_get keys i with
      | "success" -> 
        let json = Js.Dict.unsafeGet json "success" in
        (CrawlerTypes.Success ((decode_errors_list (Pbrt_bs.object_ json "errors_response" "Success"))) : CrawlerTypes.errors_response)
      | "error" -> 
        let json = Js.Dict.unsafeGet json "error" in
        (CrawlerTypes.Error (Pbrt_bs.string json "errors_response" "Error") : CrawlerTypes.errors_response)
      
      | _ -> loop (i - 1)
      end
  in
  loop (Array.length keys - 1)

let rec decode_project json =
  let v = default_project_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "full_path" -> 
      let json = Js.Dict.unsafeGet json "full_path" in
      v.full_path <- Pbrt_bs.string json "project" "full_path"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    CrawlerTypes.full_path = v.full_path;
  } : CrawlerTypes.project)

let rec decode_add_doc_request json =
  let v = default_add_doc_request_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "index" -> 
      let json = Js.Dict.unsafeGet json "index" in
      v.index <- Pbrt_bs.string json "add_doc_request" "index"
    | "crawler" -> 
      let json = Js.Dict.unsafeGet json "crawler" in
      v.crawler <- Pbrt_bs.string json "add_doc_request" "crawler"
    | "apikey" -> 
      let json = Js.Dict.unsafeGet json "apikey" in
      v.apikey <- Pbrt_bs.string json "add_doc_request" "apikey"
    | "entity" -> 
      let json = Js.Dict.unsafeGet json "entity" in
      v.entity <- Some ((decode_entity (Pbrt_bs.object_ json "add_doc_request" "entity")))
    | "changes" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "changes" in 
        Pbrt_bs.array_ a "add_doc_request" "changes"
      in
      v.changes <- Array.map (fun json -> 
        (ChangeBs.decode_change (Pbrt_bs.object_ json "add_doc_request" "changes"))
      ) a |> Array.to_list;
    end
    | "events" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "events" in 
        Pbrt_bs.array_ a "add_doc_request" "events"
      in
      v.events <- Array.map (fun json -> 
        (ChangeBs.decode_change_event (Pbrt_bs.object_ json "add_doc_request" "events"))
      ) a |> Array.to_list;
    end
    | "projects" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "projects" in 
        Pbrt_bs.array_ a "add_doc_request" "projects"
      in
      v.projects <- Array.map (fun json -> 
        (decode_project (Pbrt_bs.object_ json "add_doc_request" "projects"))
      ) a |> Array.to_list;
    end
    | "task_datas" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "task_datas" in 
        Pbrt_bs.array_ a "add_doc_request" "task_datas"
      in
      v.task_datas <- Array.map (fun json -> 
        (SearchBs.decode_task_data (Pbrt_bs.object_ json "add_doc_request" "task_datas"))
      ) a |> Array.to_list;
    end
    | "issues" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "issues" in 
        Pbrt_bs.array_ a "add_doc_request" "issues"
      in
      v.issues <- Array.map (fun json -> 
        (IssueBs.decode_issue (Pbrt_bs.object_ json "add_doc_request" "issues"))
      ) a |> Array.to_list;
    end
    | "issue_events" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "issue_events" in 
        Pbrt_bs.array_ a "add_doc_request" "issue_events"
      in
      v.issue_events <- Array.map (fun json -> 
        (IssueBs.decode_issue_event (Pbrt_bs.object_ json "add_doc_request" "issue_events"))
      ) a |> Array.to_list;
    end
    | "errors" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "errors" in 
        Pbrt_bs.array_ a "add_doc_request" "errors"
      in
      v.errors <- Array.map (fun json -> 
        (decode_crawler_error (Pbrt_bs.object_ json "add_doc_request" "errors"))
      ) a |> Array.to_list;
    end
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    CrawlerTypes.index = v.index;
    CrawlerTypes.crawler = v.crawler;
    CrawlerTypes.apikey = v.apikey;
    CrawlerTypes.entity = v.entity;
    CrawlerTypes.changes = v.changes;
    CrawlerTypes.events = v.events;
    CrawlerTypes.projects = v.projects;
    CrawlerTypes.task_datas = v.task_datas;
    CrawlerTypes.issues = v.issues;
    CrawlerTypes.issue_events = v.issue_events;
    CrawlerTypes.errors = v.errors;
  } : CrawlerTypes.add_doc_request)

let rec decode_add_doc_error (json:Js.Json.t) =
  match Pbrt_bs.string json "add_doc_error" "value" with
  | "AddUnknownIndex" -> (CrawlerTypes.Add_unknown_index : CrawlerTypes.add_doc_error)
  | "AddUnknownCrawler" -> (CrawlerTypes.Add_unknown_crawler : CrawlerTypes.add_doc_error)
  | "AddUnknownApiKey" -> (CrawlerTypes.Add_unknown_api_key : CrawlerTypes.add_doc_error)
  | "AddFailed" -> (CrawlerTypes.Add_failed : CrawlerTypes.add_doc_error)
  | "" -> CrawlerTypes.Add_unknown_index
  | _ -> Pbrt_bs.E.malformed_variant "add_doc_error"

let rec decode_add_doc_response json =
  let keys = Js.Dict.keys json in
  let rec loop = function 
    | -1 -> Pbrt_bs.E.malformed_variant "add_doc_response"
    | i -> 
      begin match Array.unsafe_get keys i with
      | "error" -> 
        let json = Js.Dict.unsafeGet json "error" in
        (CrawlerTypes.Error ((decode_add_doc_error json)) : CrawlerTypes.add_doc_response)
      
      | _ -> loop (i - 1)
      end
  in
  loop (Array.length keys - 1)

let rec decode_commit_request json =
  let v = default_commit_request_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "index" -> 
      let json = Js.Dict.unsafeGet json "index" in
      v.index <- Pbrt_bs.string json "commit_request" "index"
    | "crawler" -> 
      let json = Js.Dict.unsafeGet json "crawler" in
      v.crawler <- Pbrt_bs.string json "commit_request" "crawler"
    | "apikey" -> 
      let json = Js.Dict.unsafeGet json "apikey" in
      v.apikey <- Pbrt_bs.string json "commit_request" "apikey"
    | "entity" -> 
      let json = Js.Dict.unsafeGet json "entity" in
      v.entity <- Some ((decode_entity (Pbrt_bs.object_ json "commit_request" "entity")))
    | "timestamp" -> 
      let json = Js.Dict.unsafeGet json "timestamp" in
      v.timestamp <- Some ((TimestampBs.decode_timestamp (Pbrt_bs.string json "commit_request" "timestamp")))
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    CrawlerTypes.index = v.index;
    CrawlerTypes.crawler = v.crawler;
    CrawlerTypes.apikey = v.apikey;
    CrawlerTypes.entity = v.entity;
    CrawlerTypes.timestamp = v.timestamp;
  } : CrawlerTypes.commit_request)

let rec decode_commit_error (json:Js.Json.t) =
  match Pbrt_bs.string json "commit_error" "value" with
  | "CommitUnknownIndex" -> (CrawlerTypes.Commit_unknown_index : CrawlerTypes.commit_error)
  | "CommitUnknownCrawler" -> (CrawlerTypes.Commit_unknown_crawler : CrawlerTypes.commit_error)
  | "CommitUnknownApiKey" -> (CrawlerTypes.Commit_unknown_api_key : CrawlerTypes.commit_error)
  | "CommitDateInferiorThanPrevious" -> (CrawlerTypes.Commit_date_inferior_than_previous : CrawlerTypes.commit_error)
  | "CommitDateMissing" -> (CrawlerTypes.Commit_date_missing : CrawlerTypes.commit_error)
  | "" -> CrawlerTypes.Commit_unknown_index
  | _ -> Pbrt_bs.E.malformed_variant "commit_error"

let rec decode_commit_response json =
  let keys = Js.Dict.keys json in
  let rec loop = function 
    | -1 -> Pbrt_bs.E.malformed_variant "commit_response"
    | i -> 
      begin match Array.unsafe_get keys i with
      | "error" -> 
        let json = Js.Dict.unsafeGet json "error" in
        (CrawlerTypes.Error ((decode_commit_error json)) : CrawlerTypes.commit_response)
      | "timestamp" -> 
        let json = Js.Dict.unsafeGet json "timestamp" in
        (CrawlerTypes.Timestamp ((TimestampBs.decode_timestamp (Pbrt_bs.string json "commit_response" "Timestamp"))) : CrawlerTypes.commit_response)
      
      | _ -> loop (i - 1)
      end
  in
  loop (Array.length keys - 1)

let rec decode_commit_info_request json =
  let v = default_commit_info_request_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "index" -> 
      let json = Js.Dict.unsafeGet json "index" in
      v.index <- Pbrt_bs.string json "commit_info_request" "index"
    | "crawler" -> 
      let json = Js.Dict.unsafeGet json "crawler" in
      v.crawler <- Pbrt_bs.string json "commit_info_request" "crawler"
    | "entity" -> 
      let json = Js.Dict.unsafeGet json "entity" in
      v.entity <- (decode_entity_type json)
    | "offset" -> 
      let json = Js.Dict.unsafeGet json "offset" in
      v.offset <- Pbrt_bs.int32 json "commit_info_request" "offset"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    CrawlerTypes.index = v.index;
    CrawlerTypes.crawler = v.crawler;
    CrawlerTypes.entity = v.entity;
    CrawlerTypes.offset = v.offset;
  } : CrawlerTypes.commit_info_request)

let rec decode_commit_info_error (json:Js.Json.t) =
  match Pbrt_bs.string json "commit_info_error" "value" with
  | "CommitGetUnknownIndex" -> (CrawlerTypes.Commit_get_unknown_index : CrawlerTypes.commit_info_error)
  | "CommitGetUnknownCrawler" -> (CrawlerTypes.Commit_get_unknown_crawler : CrawlerTypes.commit_info_error)
  | "CommitGetNoEntity" -> (CrawlerTypes.Commit_get_no_entity : CrawlerTypes.commit_info_error)
  | "" -> CrawlerTypes.Commit_get_unknown_index
  | _ -> Pbrt_bs.E.malformed_variant "commit_info_error"

let rec decode_commit_info_response_oldest_entity json =
  let v = default_commit_info_response_oldest_entity_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "entity" -> 
      let json = Js.Dict.unsafeGet json "entity" in
      v.entity <- Some ((decode_entity (Pbrt_bs.object_ json "commit_info_response_oldest_entity" "entity")))
    | "last_commit_at" -> 
      let json = Js.Dict.unsafeGet json "last_commit_at" in
      v.last_commit_at <- Some ((TimestampBs.decode_timestamp (Pbrt_bs.string json "commit_info_response_oldest_entity" "last_commit_at")))
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    CrawlerTypes.entity = v.entity;
    CrawlerTypes.last_commit_at = v.last_commit_at;
  } : CrawlerTypes.commit_info_response_oldest_entity)

let rec decode_commit_info_response json =
  let keys = Js.Dict.keys json in
  let rec loop = function 
    | -1 -> Pbrt_bs.E.malformed_variant "commit_info_response"
    | i -> 
      begin match Array.unsafe_get keys i with
      | "error" -> 
        let json = Js.Dict.unsafeGet json "error" in
        (CrawlerTypes.Error ((decode_commit_info_error json)) : CrawlerTypes.commit_info_response)
      | "entity" -> 
        let json = Js.Dict.unsafeGet json "entity" in
        (CrawlerTypes.Entity ((decode_commit_info_response_oldest_entity (Pbrt_bs.object_ json "commit_info_response" "Entity"))) : CrawlerTypes.commit_info_response)
      
      | _ -> loop (i - 1)
      end
  in
  loop (Array.length keys - 1)

let rec encode_entity (v:CrawlerTypes.entity) = 
  let json = Js.Dict.empty () in
  begin match v with
  | CrawlerTypes.Organization_name v ->
    Js.Dict.set json "organization_name" (Js.Json.string v);
  | CrawlerTypes.Project_name v ->
    Js.Dict.set json "project_name" (Js.Json.string v);
  | CrawlerTypes.Project_issue_name v ->
    Js.Dict.set json "project_issueName" (Js.Json.string v);
  | CrawlerTypes.Td_name v ->
    Js.Dict.set json "td_name" (Js.Json.string v);
  | CrawlerTypes.User_name v ->
    Js.Dict.set json "user_name" (Js.Json.string v);
  end;
  json

let rec encode_entity_type (v:CrawlerTypes.entity_type) : string = 
  match v with
  | CrawlerTypes.Entity_type_organization -> "ENTITY_TYPE_ORGANIZATION"
  | CrawlerTypes.Entity_type_project -> "ENTITY_TYPE_PROJECT"
  | CrawlerTypes.Entity_type_task_data -> "ENTITY_TYPE_TASK_DATA"
  | CrawlerTypes.Entity_type_user -> "ENTITY_TYPE_USER"

let rec encode_crawler_error (v:CrawlerTypes.crawler_error) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "message" (Js.Json.string v.CrawlerTypes.message);
  Js.Dict.set json "body" (Js.Json.string v.CrawlerTypes.body);
  begin match v.CrawlerTypes.created_at with
  | None -> ()
  | Some v ->
    begin (* created_at field *)
      let json' = TimestampBs.encode_timestamp v in
      Js.Dict.set json "created_at" (Js.Json.string json');
    end;
  end;
  json

let rec encode_crawler_error_list (v:CrawlerTypes.crawler_error_list) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "crawler" (Js.Json.string v.CrawlerTypes.crawler);
  begin match v.CrawlerTypes.entity with
  | None -> ()
  | Some v ->
    begin (* entity field *)
      let json' = encode_entity v in
      Js.Dict.set json "entity" (Js.Json.object_ json');
    end;
  end;
  begin match v.CrawlerTypes.errors with
  | [] -> ()
  | __x__ -> (* errors *)
    let (errors':Js.Json.t) =
      __x__
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_crawler_error |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "errors" errors'
  end;
  json

let rec encode_errors_request (v:CrawlerTypes.errors_request) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "index" (Js.Json.string v.CrawlerTypes.index);
  Js.Dict.set json "query" (Js.Json.string v.CrawlerTypes.query);
  json

let rec encode_errors_list (v:CrawlerTypes.errors_list) = 
  let json = Js.Dict.empty () in
  begin match v.CrawlerTypes.errors with
  | [] -> ()
  | __x__ -> (* errors *)
    let (errors':Js.Json.t) =
      __x__
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_crawler_error_list |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "errors" errors'
  end;
  json

let rec encode_errors_response (v:CrawlerTypes.errors_response) = 
  let json = Js.Dict.empty () in
  begin match v with
  | CrawlerTypes.Success v ->
    begin (* success field *)
      let json' = encode_errors_list v in
      Js.Dict.set json "success" (Js.Json.object_ json');
    end;
  | CrawlerTypes.Error v ->
    Js.Dict.set json "error" (Js.Json.string v);
  end;
  json

let rec encode_project (v:CrawlerTypes.project) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "full_path" (Js.Json.string v.CrawlerTypes.full_path);
  json

let rec encode_add_doc_request (v:CrawlerTypes.add_doc_request) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "index" (Js.Json.string v.CrawlerTypes.index);
  Js.Dict.set json "crawler" (Js.Json.string v.CrawlerTypes.crawler);
  Js.Dict.set json "apikey" (Js.Json.string v.CrawlerTypes.apikey);
  begin match v.CrawlerTypes.entity with
  | None -> ()
  | Some v ->
    begin (* entity field *)
      let json' = encode_entity v in
      Js.Dict.set json "entity" (Js.Json.object_ json');
    end;
  end;
  begin match v.CrawlerTypes.changes with
  | [] -> ()
  | __x__ -> (* changes *)
    let (changes':Js.Json.t) =
      __x__
      |> Array.of_list
      |> Array.map (fun v ->
        v |> ChangeBs.encode_change |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "changes" changes'
  end;
  begin match v.CrawlerTypes.events with
  | [] -> ()
  | __x__ -> (* events *)
    let (events':Js.Json.t) =
      __x__
      |> Array.of_list
      |> Array.map (fun v ->
        v |> ChangeBs.encode_change_event |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "events" events'
  end;
  begin match v.CrawlerTypes.projects with
  | [] -> ()
  | __x__ -> (* projects *)
    let (projects':Js.Json.t) =
      __x__
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_project |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "projects" projects'
  end;
  begin match v.CrawlerTypes.task_datas with
  | [] -> ()
  | __x__ -> (* taskDatas *)
    let (task_datas':Js.Json.t) =
      __x__
      |> Array.of_list
      |> Array.map (fun v ->
        v |> SearchBs.encode_task_data |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "task_datas" task_datas'
  end;
  begin match v.CrawlerTypes.issues with
  | [] -> ()
  | __x__ -> (* issues *)
    let (issues':Js.Json.t) =
      __x__
      |> Array.of_list
      |> Array.map (fun v ->
        v |> IssueBs.encode_issue |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "issues" issues'
  end;
  begin match v.CrawlerTypes.issue_events with
  | [] -> ()
  | __x__ -> (* issueEvents *)
    let (issue_events':Js.Json.t) =
      __x__
      |> Array.of_list
      |> Array.map (fun v ->
        v |> IssueBs.encode_issue_event |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "issue_events" issue_events'
  end;
  begin match v.CrawlerTypes.errors with
  | [] -> ()
  | __x__ -> (* errors *)
    let (errors':Js.Json.t) =
      __x__
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_crawler_error |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "errors" errors'
  end;
  json

let rec encode_add_doc_error (v:CrawlerTypes.add_doc_error) : string = 
  match v with
  | CrawlerTypes.Add_unknown_index -> "AddUnknownIndex"
  | CrawlerTypes.Add_unknown_crawler -> "AddUnknownCrawler"
  | CrawlerTypes.Add_unknown_api_key -> "AddUnknownApiKey"
  | CrawlerTypes.Add_failed -> "AddFailed"

let rec encode_add_doc_response (v:CrawlerTypes.add_doc_response) = 
  let json = Js.Dict.empty () in
  begin match v with
  | CrawlerTypes.Error v ->
    Js.Dict.set json "error" (Js.Json.string (encode_add_doc_error v));
  end;
  json

let rec encode_commit_request (v:CrawlerTypes.commit_request) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "index" (Js.Json.string v.CrawlerTypes.index);
  Js.Dict.set json "crawler" (Js.Json.string v.CrawlerTypes.crawler);
  Js.Dict.set json "apikey" (Js.Json.string v.CrawlerTypes.apikey);
  begin match v.CrawlerTypes.entity with
  | None -> ()
  | Some v ->
    begin (* entity field *)
      let json' = encode_entity v in
      Js.Dict.set json "entity" (Js.Json.object_ json');
    end;
  end;
  begin match v.CrawlerTypes.timestamp with
  | None -> ()
  | Some v ->
    begin (* timestamp field *)
      let json' = TimestampBs.encode_timestamp v in
      Js.Dict.set json "timestamp" (Js.Json.string json');
    end;
  end;
  json

let rec encode_commit_error (v:CrawlerTypes.commit_error) : string = 
  match v with
  | CrawlerTypes.Commit_unknown_index -> "CommitUnknownIndex"
  | CrawlerTypes.Commit_unknown_crawler -> "CommitUnknownCrawler"
  | CrawlerTypes.Commit_unknown_api_key -> "CommitUnknownApiKey"
  | CrawlerTypes.Commit_date_inferior_than_previous -> "CommitDateInferiorThanPrevious"
  | CrawlerTypes.Commit_date_missing -> "CommitDateMissing"

let rec encode_commit_response (v:CrawlerTypes.commit_response) = 
  let json = Js.Dict.empty () in
  begin match v with
  | CrawlerTypes.Error v ->
    Js.Dict.set json "error" (Js.Json.string (encode_commit_error v));
  | CrawlerTypes.Timestamp v ->
    begin (* timestamp field *)
      let json' = TimestampBs.encode_timestamp v in
      Js.Dict.set json "timestamp" (Js.Json.string json');
    end;
  end;
  json

let rec encode_commit_info_request (v:CrawlerTypes.commit_info_request) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "index" (Js.Json.string v.CrawlerTypes.index);
  Js.Dict.set json "crawler" (Js.Json.string v.CrawlerTypes.crawler);
  Js.Dict.set json "entity" (Js.Json.string (encode_entity_type v.CrawlerTypes.entity));
  Js.Dict.set json "offset" (Js.Json.number (Int32.to_float v.CrawlerTypes.offset));
  json

let rec encode_commit_info_error (v:CrawlerTypes.commit_info_error) : string = 
  match v with
  | CrawlerTypes.Commit_get_unknown_index -> "CommitGetUnknownIndex"
  | CrawlerTypes.Commit_get_unknown_crawler -> "CommitGetUnknownCrawler"
  | CrawlerTypes.Commit_get_no_entity -> "CommitGetNoEntity"

let rec encode_commit_info_response_oldest_entity (v:CrawlerTypes.commit_info_response_oldest_entity) = 
  let json = Js.Dict.empty () in
  begin match v.CrawlerTypes.entity with
  | None -> ()
  | Some v ->
    begin (* entity field *)
      let json' = encode_entity v in
      Js.Dict.set json "entity" (Js.Json.object_ json');
    end;
  end;
  begin match v.CrawlerTypes.last_commit_at with
  | None -> ()
  | Some v ->
    begin (* lastCommitAt field *)
      let json' = TimestampBs.encode_timestamp v in
      Js.Dict.set json "last_commit_at" (Js.Json.string json');
    end;
  end;
  json

let rec encode_commit_info_response (v:CrawlerTypes.commit_info_response) = 
  let json = Js.Dict.empty () in
  begin match v with
  | CrawlerTypes.Error v ->
    Js.Dict.set json "error" (Js.Json.string (encode_commit_info_error v));
  | CrawlerTypes.Entity v ->
    begin (* entity field *)
      let json' = encode_commit_info_response_oldest_entity v in
      Js.Dict.set json "entity" (Js.Json.object_ json');
    end;
  end;
  json
