@@ocaml.warning("-27-30-39")

type project_definition = {
  name: string,
  repository_regex: string,
  branch_regex: string,
  file_regex: string,
}

type get_projects_request = {index: string}

type get_projects_response = {projects: list<project_definition>}

let rec default_project_definition = (
  ~name: string="",
  ~repository_regex: string="",
  ~branch_regex: string="",
  ~file_regex: string="",
  (),
): project_definition => {
  name: name,
  repository_regex: repository_regex,
  branch_regex: branch_regex,
  file_regex: file_regex,
}

let rec default_get_projects_request = (~index: string="", ()): get_projects_request => {
  index: index,
}

let rec default_get_projects_response = (
  ~projects: list<project_definition>=list{},
  (),
): get_projects_response => {
  projects: projects,
}
