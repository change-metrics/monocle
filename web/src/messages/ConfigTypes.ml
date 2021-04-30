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
