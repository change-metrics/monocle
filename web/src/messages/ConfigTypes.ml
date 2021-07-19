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

type health_request = {
  index : string;
}

type health_response = {
  status : string;
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

let rec default_health_request 
  ?index:((index:string) = "")
  () : health_request  = {
  index;
}

let rec default_health_response 
  ?status:((status:string) = "")
  () : health_response  = {
  status;
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
