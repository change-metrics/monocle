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

type health_request = {
  index : string;
}

type health_response = {
  status : string;
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

val default_health_request : 
  ?index:string ->
  unit ->
  health_request
(** [default_health_request ()] is the default value for type [health_request] *)

val default_health_response : 
  ?status:string ->
  unit ->
  health_response
(** [default_health_response ()] is the default value for type [health_response] *)
