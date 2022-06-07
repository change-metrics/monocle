[@@@ocaml.warning "-27-30-39"]


type project_definition = {
  name : string;
  repository_regex : string;
  branch_regex : string;
  file_regex : string;
}

type get_projects_request = {
  index : string;
}

type get_projects_response = {
  projects : project_definition list;
}

type workspace = {
  name : string;
}

type get_workspaces_request = {
  void : string;
}

type get_workspaces_response = {
  workspaces : workspace list;
}

type about_about_link = {
  name : string;
  url : string;
  category : string;
}

type about = {
  version : string;
  links : about_about_link list;
  auth_enabled : bool;
}

type get_about_request = {
  void : string;
}

type get_about_response = {
  about : about option;
}

type group_definition = {
  name : string;
  members : int32;
}

type get_groups_request = {
  index : string;
}

type get_groups_response = {
  items : group_definition list;
}

type get_group_members_request = {
  index : string;
  group : string;
}

type get_group_members_response = {
  members : string list;
}

let rec default_project_definition 
  ?name:((name:string) = "")
  ?repository_regex:((repository_regex:string) = "")
  ?branch_regex:((branch_regex:string) = "")
  ?file_regex:((file_regex:string) = "")
  () : project_definition  = {
  name;
  repository_regex;
  branch_regex;
  file_regex;
}

let rec default_get_projects_request 
  ?index:((index:string) = "")
  () : get_projects_request  = {
  index;
}

let rec default_get_projects_response 
  ?projects:((projects:project_definition list) = [])
  () : get_projects_response  = {
  projects;
}

let rec default_workspace 
  ?name:((name:string) = "")
  () : workspace  = {
  name;
}

let rec default_get_workspaces_request 
  ?void:((void:string) = "")
  () : get_workspaces_request  = {
  void;
}

let rec default_get_workspaces_response 
  ?workspaces:((workspaces:workspace list) = [])
  () : get_workspaces_response  = {
  workspaces;
}

let rec default_about_about_link 
  ?name:((name:string) = "")
  ?url:((url:string) = "")
  ?category:((category:string) = "")
  () : about_about_link  = {
  name;
  url;
  category;
}

let rec default_about 
  ?version:((version:string) = "")
  ?links:((links:about_about_link list) = [])
  ?auth_enabled:((auth_enabled:bool) = false)
  () : about  = {
  version;
  links;
  auth_enabled;
}

let rec default_get_about_request 
  ?void:((void:string) = "")
  () : get_about_request  = {
  void;
}

let rec default_get_about_response 
  ?about:((about:about option) = None)
  () : get_about_response  = {
  about;
}

let rec default_group_definition 
  ?name:((name:string) = "")
  ?members:((members:int32) = 0l)
  () : group_definition  = {
  name;
  members;
}

let rec default_get_groups_request 
  ?index:((index:string) = "")
  () : get_groups_request  = {
  index;
}

let rec default_get_groups_response 
  ?items:((items:group_definition list) = [])
  () : get_groups_response  = {
  items;
}

let rec default_get_group_members_request 
  ?index:((index:string) = "")
  ?group:((group:string) = "")
  () : get_group_members_request  = {
  index;
  group;
}

let rec default_get_group_members_response 
  ?members:((members:string list) = [])
  () : get_group_members_response  = {
  members;
}
