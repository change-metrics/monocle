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

type about_about_link = {
  name : string;
  url : string;
  category : string;
}

type about_auth_config = {
  force_login : bool;
  issuer : string;
  provider_name : string;
}

type about_auth =
  | Auth_config of about_auth_config

and about = {
  version : string;
  links : about_about_link list;
  auth : about_auth;
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

val default_about_about_link : 
  ?name:string ->
  ?url:string ->
  ?category:string ->
  unit ->
  about_about_link
(** [default_about_about_link ()] is the default value for type [about_about_link] *)

val default_about_auth_config : 
  ?force_login:bool ->
  ?issuer:string ->
  ?provider_name:string ->
  unit ->
  about_auth_config
(** [default_about_auth_config ()] is the default value for type [about_auth_config] *)

val default_about_auth : unit -> about_auth
(** [default_about_auth ()] is the default value for type [about_auth] *)

val default_about : 
  ?version:string ->
  ?links:about_about_link list ->
  ?auth:about_auth ->
  unit ->
  about
(** [default_about ()] is the default value for type [about] *)

val default_get_about_request : 
  ?void:string ->
  unit ->
  get_about_request
(** [default_get_about_request ()] is the default value for type [get_about_request] *)

val default_get_about_response : 
  ?about:about option ->
  unit ->
  get_about_response
(** [default_get_about_response ()] is the default value for type [get_about_response] *)

val default_group_definition : 
  ?name:string ->
  ?members:int32 ->
  unit ->
  group_definition
(** [default_group_definition ()] is the default value for type [group_definition] *)

val default_get_groups_request : 
  ?index:string ->
  unit ->
  get_groups_request
(** [default_get_groups_request ()] is the default value for type [get_groups_request] *)

val default_get_groups_response : 
  ?items:group_definition list ->
  unit ->
  get_groups_response
(** [default_get_groups_response ()] is the default value for type [get_groups_response] *)

val default_get_group_members_request : 
  ?index:string ->
  ?group:string ->
  unit ->
  get_group_members_request
(** [default_get_group_members_request ()] is the default value for type [get_group_members_request] *)

val default_get_group_members_response : 
  ?members:string list ->
  unit ->
  get_group_members_response
(** [default_get_group_members_response ()] is the default value for type [get_group_members_response] *)
