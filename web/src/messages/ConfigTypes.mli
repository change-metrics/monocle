(** config.proto Types *)



(** {2 Types} *)

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


(** {2 Default values} *)

val default_project_definition : 
  ?name:string ->
  ?repository_regex:string ->
  ?branch_regex:string ->
  ?file_regex:string ->
  unit ->
  project_definition
(** [default_project_definition ()] is the default value for type [project_definition] *)

val default_get_projects_request : 
  ?index:string ->
  unit ->
  get_projects_request
(** [default_get_projects_request ()] is the default value for type [get_projects_request] *)

val default_get_projects_response : 
  ?projects:project_definition list ->
  unit ->
  get_projects_response
(** [default_get_projects_response ()] is the default value for type [get_projects_response] *)

val default_workspace : 
  ?name:string ->
  unit ->
  workspace
(** [default_workspace ()] is the default value for type [workspace] *)

val default_get_workspaces_request : 
  ?void:string ->
  unit ->
  get_workspaces_request
(** [default_get_workspaces_request ()] is the default value for type [get_workspaces_request] *)

val default_get_workspaces_response : 
  ?workspaces:workspace list ->
  unit ->
  get_workspaces_response
(** [default_get_workspaces_response ()] is the default value for type [get_workspaces_response] *)
