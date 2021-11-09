[@@@ocaml.warning "-27-30-39"]

type project_definition_mutable = {
  mutable name : string;
  mutable repository_regex : string;
  mutable branch_regex : string;
  mutable file_regex : string;
}

let default_project_definition_mutable () : project_definition_mutable = {
  name = "";
  repository_regex = "";
  branch_regex = "";
  file_regex = "";
}

type get_projects_request_mutable = {
  mutable index : string;
}

let default_get_projects_request_mutable () : get_projects_request_mutable = {
  index = "";
}

type get_projects_response_mutable = {
  mutable projects : ConfigTypes.project_definition list;
}

let default_get_projects_response_mutable () : get_projects_response_mutable = {
  projects = [];
}

type workspace_mutable = {
  mutable name : string;
}

let default_workspace_mutable () : workspace_mutable = {
  name = "";
}

type get_workspaces_request_mutable = {
  mutable void : string;
}

let default_get_workspaces_request_mutable () : get_workspaces_request_mutable = {
  void = "";
}

type get_workspaces_response_mutable = {
  mutable workspaces : ConfigTypes.workspace list;
}

let default_get_workspaces_response_mutable () : get_workspaces_response_mutable = {
  workspaces = [];
}

type about_about_link_mutable = {
  mutable name : string;
  mutable url : string;
  mutable category : string;
}

let default_about_about_link_mutable () : about_about_link_mutable = {
  name = "";
  url = "";
  category = "";
}

type about_mutable = {
  mutable version : string;
  mutable links : ConfigTypes.about_about_link list;
}

let default_about_mutable () : about_mutable = {
  version = "";
  links = [];
}

type get_about_request_mutable = {
  mutable void : string;
}

let default_get_about_request_mutable () : get_about_request_mutable = {
  void = "";
}

type get_about_response_mutable = {
  mutable about : ConfigTypes.about option;
}

let default_get_about_response_mutable () : get_about_response_mutable = {
  about = None;
}


let rec decode_project_definition json =
  let v = default_project_definition_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "name" -> 
      let json = Js.Dict.unsafeGet json "name" in
      v.name <- Pbrt_bs.string json "project_definition" "name"
    | "repository_regex" -> 
      let json = Js.Dict.unsafeGet json "repository_regex" in
      v.repository_regex <- Pbrt_bs.string json "project_definition" "repository_regex"
    | "branch_regex" -> 
      let json = Js.Dict.unsafeGet json "branch_regex" in
      v.branch_regex <- Pbrt_bs.string json "project_definition" "branch_regex"
    | "file_regex" -> 
      let json = Js.Dict.unsafeGet json "file_regex" in
      v.file_regex <- Pbrt_bs.string json "project_definition" "file_regex"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    ConfigTypes.name = v.name;
    ConfigTypes.repository_regex = v.repository_regex;
    ConfigTypes.branch_regex = v.branch_regex;
    ConfigTypes.file_regex = v.file_regex;
  } : ConfigTypes.project_definition)

let rec decode_get_projects_request json =
  let v = default_get_projects_request_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "index" -> 
      let json = Js.Dict.unsafeGet json "index" in
      v.index <- Pbrt_bs.string json "get_projects_request" "index"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    ConfigTypes.index = v.index;
  } : ConfigTypes.get_projects_request)

let rec decode_get_projects_response json =
  let v = default_get_projects_response_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "projects" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "projects" in 
        Pbrt_bs.array_ a "get_projects_response" "projects"
      in
      v.projects <- Array.map (fun json -> 
        (decode_project_definition (Pbrt_bs.object_ json "get_projects_response" "projects"))
      ) a |> Array.to_list;
    end
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    ConfigTypes.projects = v.projects;
  } : ConfigTypes.get_projects_response)

let rec decode_workspace json =
  let v = default_workspace_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "name" -> 
      let json = Js.Dict.unsafeGet json "name" in
      v.name <- Pbrt_bs.string json "workspace" "name"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    ConfigTypes.name = v.name;
  } : ConfigTypes.workspace)

let rec decode_get_workspaces_request json =
  let v = default_get_workspaces_request_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "void" -> 
      let json = Js.Dict.unsafeGet json "void" in
      v.void <- Pbrt_bs.string json "get_workspaces_request" "void"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    ConfigTypes.void = v.void;
  } : ConfigTypes.get_workspaces_request)

let rec decode_get_workspaces_response json =
  let v = default_get_workspaces_response_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "workspaces" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "workspaces" in 
        Pbrt_bs.array_ a "get_workspaces_response" "workspaces"
      in
      v.workspaces <- Array.map (fun json -> 
        (decode_workspace (Pbrt_bs.object_ json "get_workspaces_response" "workspaces"))
      ) a |> Array.to_list;
    end
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    ConfigTypes.workspaces = v.workspaces;
  } : ConfigTypes.get_workspaces_response)

let rec decode_about_about_link json =
  let v = default_about_about_link_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "name" -> 
      let json = Js.Dict.unsafeGet json "name" in
      v.name <- Pbrt_bs.string json "about_about_link" "name"
    | "url" -> 
      let json = Js.Dict.unsafeGet json "url" in
      v.url <- Pbrt_bs.string json "about_about_link" "url"
    | "category" -> 
      let json = Js.Dict.unsafeGet json "category" in
      v.category <- Pbrt_bs.string json "about_about_link" "category"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    ConfigTypes.name = v.name;
    ConfigTypes.url = v.url;
    ConfigTypes.category = v.category;
  } : ConfigTypes.about_about_link)

let rec decode_about json =
  let v = default_about_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "version" -> 
      let json = Js.Dict.unsafeGet json "version" in
      v.version <- Pbrt_bs.string json "about" "version"
    | "links" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "links" in 
        Pbrt_bs.array_ a "about" "links"
      in
      v.links <- Array.map (fun json -> 
        (decode_about_about_link (Pbrt_bs.object_ json "about" "links"))
      ) a |> Array.to_list;
    end
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    ConfigTypes.version = v.version;
    ConfigTypes.links = v.links;
  } : ConfigTypes.about)

let rec decode_get_about_request json =
  let v = default_get_about_request_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "void" -> 
      let json = Js.Dict.unsafeGet json "void" in
      v.void <- Pbrt_bs.string json "get_about_request" "void"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    ConfigTypes.void = v.void;
  } : ConfigTypes.get_about_request)

let rec decode_get_about_response json =
  let v = default_get_about_response_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "about" -> 
      let json = Js.Dict.unsafeGet json "about" in
      v.about <- Some ((decode_about (Pbrt_bs.object_ json "get_about_response" "about")))
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    ConfigTypes.about = v.about;
  } : ConfigTypes.get_about_response)

let rec encode_project_definition (v:ConfigTypes.project_definition) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "name" (Js.Json.string v.ConfigTypes.name);
  Js.Dict.set json "repository_regex" (Js.Json.string v.ConfigTypes.repository_regex);
  Js.Dict.set json "branch_regex" (Js.Json.string v.ConfigTypes.branch_regex);
  Js.Dict.set json "file_regex" (Js.Json.string v.ConfigTypes.file_regex);
  json

let rec encode_get_projects_request (v:ConfigTypes.get_projects_request) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "index" (Js.Json.string v.ConfigTypes.index);
  json

let rec encode_get_projects_response (v:ConfigTypes.get_projects_response) = 
  let json = Js.Dict.empty () in
  begin (* projects field *)
    let (projects':Js.Json.t) =
      v.ConfigTypes.projects
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_project_definition |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "projects" projects';
  end;
  json

let rec encode_workspace (v:ConfigTypes.workspace) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "name" (Js.Json.string v.ConfigTypes.name);
  json

let rec encode_get_workspaces_request (v:ConfigTypes.get_workspaces_request) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "void" (Js.Json.string v.ConfigTypes.void);
  json

let rec encode_get_workspaces_response (v:ConfigTypes.get_workspaces_response) = 
  let json = Js.Dict.empty () in
  begin (* workspaces field *)
    let (workspaces':Js.Json.t) =
      v.ConfigTypes.workspaces
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_workspace |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "workspaces" workspaces';
  end;
  json

let rec encode_about_about_link (v:ConfigTypes.about_about_link) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "name" (Js.Json.string v.ConfigTypes.name);
  Js.Dict.set json "url" (Js.Json.string v.ConfigTypes.url);
  Js.Dict.set json "category" (Js.Json.string v.ConfigTypes.category);
  json

let rec encode_about (v:ConfigTypes.about) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "version" (Js.Json.string v.ConfigTypes.version);
  begin (* links field *)
    let (links':Js.Json.t) =
      v.ConfigTypes.links
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_about_about_link |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "links" links';
  end;
  json

let rec encode_get_about_request (v:ConfigTypes.get_about_request) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "void" (Js.Json.string v.ConfigTypes.void);
  json

let rec encode_get_about_response (v:ConfigTypes.get_about_response) = 
  let json = Js.Dict.empty () in
  begin match v.ConfigTypes.about with
  | None -> ()
  | Some v ->
    begin (* about field *)
      let json' = encode_about v in
      Js.Dict.set json "about" (Js.Json.object_ json');
    end;
  end;
  json
