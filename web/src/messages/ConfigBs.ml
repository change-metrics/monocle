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

type about_auth_config_mutable = {
  mutable force_login : bool;
  mutable issuer : string;
  mutable provider_name : string;
}

let default_about_auth_config_mutable () : about_auth_config_mutable = {
  force_login = false;
  issuer = "";
  provider_name = "";
}

type about_mutable = {
  mutable version : string;
  mutable links : ConfigTypes.about_about_link list;
  mutable auth : ConfigTypes.about_auth;
}

let default_about_mutable () : about_mutable = {
  version = "";
  links = [];
  auth = ConfigTypes.Auth_config (ConfigTypes.default_about_auth_config ());
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

type group_definition_mutable = {
  mutable name : string;
  mutable members : int32;
}

let default_group_definition_mutable () : group_definition_mutable = {
  name = "";
  members = 0l;
}

type get_groups_request_mutable = {
  mutable index : string;
}

let default_get_groups_request_mutable () : get_groups_request_mutable = {
  index = "";
}

type get_groups_response_mutable = {
  mutable items : ConfigTypes.group_definition list;
}

let default_get_groups_response_mutable () : get_groups_response_mutable = {
  items = [];
}

type get_group_members_request_mutable = {
  mutable index : string;
  mutable group : string;
}

let default_get_group_members_request_mutable () : get_group_members_request_mutable = {
  index = "";
  group = "";
}

type get_group_members_response_mutable = {
  mutable members : string list;
}

let default_get_group_members_response_mutable () : get_group_members_response_mutable = {
  members = [];
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

let rec decode_about_auth_config json =
  let v = default_about_auth_config_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "force_login" -> 
      let json = Js.Dict.unsafeGet json "force_login" in
      v.force_login <- Pbrt_bs.bool json "about_auth_config" "force_login"
    | "issuer" -> 
      let json = Js.Dict.unsafeGet json "issuer" in
      v.issuer <- Pbrt_bs.string json "about_auth_config" "issuer"
    | "provider_name" -> 
      let json = Js.Dict.unsafeGet json "provider_name" in
      v.provider_name <- Pbrt_bs.string json "about_auth_config" "provider_name"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    ConfigTypes.force_login = v.force_login;
    ConfigTypes.issuer = v.issuer;
    ConfigTypes.provider_name = v.provider_name;
  } : ConfigTypes.about_auth_config)

let rec decode_about_auth json =
  let keys = Js.Dict.keys json in
  let rec loop = function 
    | -1 -> Pbrt_bs.E.malformed_variant "about_auth"
    | i -> 
      begin match Array.unsafe_get keys i with
      | "auth_config" -> 
        let json = Js.Dict.unsafeGet json "auth_config" in
        (ConfigTypes.Auth_config ((decode_about_auth_config (Pbrt_bs.object_ json "about_auth" "Auth_config"))) : ConfigTypes.about_auth)
      
      | _ -> loop (i - 1)
      end
  in
  loop (Array.length keys - 1)

and decode_about json =
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
    | "auth_config" -> 
      let json = Js.Dict.unsafeGet json "auth_config" in
      v.auth <- Auth_config ((decode_about_auth_config (Pbrt_bs.object_ json "about" "auth")))
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    ConfigTypes.version = v.version;
    ConfigTypes.links = v.links;
    ConfigTypes.auth = v.auth;
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

let rec decode_group_definition json =
  let v = default_group_definition_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "name" -> 
      let json = Js.Dict.unsafeGet json "name" in
      v.name <- Pbrt_bs.string json "group_definition" "name"
    | "members" -> 
      let json = Js.Dict.unsafeGet json "members" in
      v.members <- Pbrt_bs.int32 json "group_definition" "members"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    ConfigTypes.name = v.name;
    ConfigTypes.members = v.members;
  } : ConfigTypes.group_definition)

let rec decode_get_groups_request json =
  let v = default_get_groups_request_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "index" -> 
      let json = Js.Dict.unsafeGet json "index" in
      v.index <- Pbrt_bs.string json "get_groups_request" "index"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    ConfigTypes.index = v.index;
  } : ConfigTypes.get_groups_request)

let rec decode_get_groups_response json =
  let v = default_get_groups_response_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "items" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "items" in 
        Pbrt_bs.array_ a "get_groups_response" "items"
      in
      v.items <- Array.map (fun json -> 
        (decode_group_definition (Pbrt_bs.object_ json "get_groups_response" "items"))
      ) a |> Array.to_list;
    end
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    ConfigTypes.items = v.items;
  } : ConfigTypes.get_groups_response)

let rec decode_get_group_members_request json =
  let v = default_get_group_members_request_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "index" -> 
      let json = Js.Dict.unsafeGet json "index" in
      v.index <- Pbrt_bs.string json "get_group_members_request" "index"
    | "group" -> 
      let json = Js.Dict.unsafeGet json "group" in
      v.group <- Pbrt_bs.string json "get_group_members_request" "group"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    ConfigTypes.index = v.index;
    ConfigTypes.group = v.group;
  } : ConfigTypes.get_group_members_request)

let rec decode_get_group_members_response json =
  let v = default_get_group_members_response_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "members" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "members" in 
        Pbrt_bs.array_ a "get_group_members_response" "members"
      in
      v.members <- Array.map (fun json -> 
        Pbrt_bs.string json "get_group_members_response" "members"
      ) a |> Array.to_list;
    end
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    ConfigTypes.members = v.members;
  } : ConfigTypes.get_group_members_response)

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
  begin match v.ConfigTypes.projects with
  | [] -> ()
  | __x__ -> (* projects *)
    let (projects':Js.Json.t) =
      __x__
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_project_definition |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "projects" projects'
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
  begin match v.ConfigTypes.workspaces with
  | [] -> ()
  | __x__ -> (* workspaces *)
    let (workspaces':Js.Json.t) =
      __x__
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_workspace |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "workspaces" workspaces'
  end;
  json

let rec encode_about_about_link (v:ConfigTypes.about_about_link) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "name" (Js.Json.string v.ConfigTypes.name);
  Js.Dict.set json "url" (Js.Json.string v.ConfigTypes.url);
  Js.Dict.set json "category" (Js.Json.string v.ConfigTypes.category);
  json

let rec encode_about_auth_config (v:ConfigTypes.about_auth_config) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "force_login" (Js.Json.boolean v.ConfigTypes.force_login);
  Js.Dict.set json "issuer" (Js.Json.string v.ConfigTypes.issuer);
  Js.Dict.set json "provider_name" (Js.Json.string v.ConfigTypes.provider_name);
  json

let rec encode_about_auth (v:ConfigTypes.about_auth) = 
  let json = Js.Dict.empty () in
  begin match v with
  | ConfigTypes.Auth_config v ->
    begin (* authConfig field *)
      let json' = encode_about_auth_config v in
      Js.Dict.set json "auth_config" (Js.Json.object_ json');
    end;
  end;
  json

and encode_about (v:ConfigTypes.about) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "version" (Js.Json.string v.ConfigTypes.version);
  begin match v.ConfigTypes.links with
  | [] -> ()
  | __x__ -> (* links *)
    let (links':Js.Json.t) =
      __x__
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_about_about_link |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "links" links'
  end;
  begin match v.ConfigTypes.auth with
    | Auth_config v ->
      begin (* authConfig field *)
        let json' = encode_about_auth_config v in
        Js.Dict.set json "auth_config" (Js.Json.object_ json');
      end;
  end; (* match v.auth *)
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

let rec encode_group_definition (v:ConfigTypes.group_definition) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "name" (Js.Json.string v.ConfigTypes.name);
  Js.Dict.set json "members" (Js.Json.number (Int32.to_float v.ConfigTypes.members));
  json

let rec encode_get_groups_request (v:ConfigTypes.get_groups_request) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "index" (Js.Json.string v.ConfigTypes.index);
  json

let rec encode_get_groups_response (v:ConfigTypes.get_groups_response) = 
  let json = Js.Dict.empty () in
  begin match v.ConfigTypes.items with
  | [] -> ()
  | __x__ -> (* items *)
    let (items':Js.Json.t) =
      __x__
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_group_definition |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "items" items'
  end;
  json

let rec encode_get_group_members_request (v:ConfigTypes.get_group_members_request) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "index" (Js.Json.string v.ConfigTypes.index);
  Js.Dict.set json "group" (Js.Json.string v.ConfigTypes.group);
  json

let rec encode_get_group_members_response (v:ConfigTypes.get_group_members_response) = 
  let json = Js.Dict.empty () in
  begin match v.ConfigTypes.members with
  | [] -> ()
  | __x__ -> (* members *)
    let a = __x__ |> Array.of_list |> Array.map Js.Json.string in
    Js.Dict.set json "members" (Js.Json.array a);
  end;
  json
