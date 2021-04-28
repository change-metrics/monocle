@@ocaml.warning("-27-30-39")

type project_definition_mutable = {
  mutable name: string,
  mutable repository_regex: string,
  mutable branch_regex: string,
  mutable file_regex: string,
}

let default_project_definition_mutable = (): project_definition_mutable => {
  name: "",
  repository_regex: "",
  branch_regex: "",
  file_regex: "",
}

type get_projects_request_mutable = {mutable index: string}

let default_get_projects_request_mutable = (): get_projects_request_mutable => {
  index: "",
}

type get_projects_response_mutable = {mutable projects: list<ConfigTypes.project_definition>}

let default_get_projects_response_mutable = (): get_projects_response_mutable => {
  projects: list{},
}

let rec decode_project_definition = json => {
  let v = default_project_definition_mutable()
  let keys = Js.Dict.keys(json)
  let last_key_index = Array.length(keys) - 1
  for i in 0 to last_key_index {
    switch Array.unsafe_get(keys, i) {
    | "name" =>
      let json = Js.Dict.unsafeGet(json, "name")
      v.name = Pbrt_bs.string(json, "project_definition", "name")
    | "repository_regex" =>
      let json = Js.Dict.unsafeGet(json, "repository_regex")
      v.repository_regex = Pbrt_bs.string(json, "project_definition", "repository_regex")
    | "branch_regex" =>
      let json = Js.Dict.unsafeGet(json, "branch_regex")
      v.branch_regex = Pbrt_bs.string(json, "project_definition", "branch_regex")
    | "file_regex" =>
      let json = Js.Dict.unsafeGet(json, "file_regex")
      v.file_regex = Pbrt_bs.string(json, "project_definition", "file_regex")

    | _ => ()
    } /* Unknown fields are ignored */
  }
  (
    {
      ConfigTypes.name: v.name,
      ConfigTypes.repository_regex: v.repository_regex,
      ConfigTypes.branch_regex: v.branch_regex,
      ConfigTypes.file_regex: v.file_regex,
    }: ConfigTypes.project_definition
  )
}

let rec decode_get_projects_request = json => {
  let v = default_get_projects_request_mutable()
  let keys = Js.Dict.keys(json)
  let last_key_index = Array.length(keys) - 1
  for i in 0 to last_key_index {
    switch Array.unsafe_get(keys, i) {
    | "index" =>
      let json = Js.Dict.unsafeGet(json, "index")
      v.index = Pbrt_bs.string(json, "get_projects_request", "index")

    | _ => ()
    } /* Unknown fields are ignored */
  }
  (
    {
      ConfigTypes.index: v.index,
    }: ConfigTypes.get_projects_request
  )
}

let rec decode_get_projects_response = json => {
  let v = default_get_projects_response_mutable()
  let keys = Js.Dict.keys(json)
  let last_key_index = Array.length(keys) - 1
  for i in 0 to last_key_index {
    switch Array.unsafe_get(keys, i) {
    | "projects" =>
      let a = {
        let a = Js.Dict.unsafeGet(json, "projects")
        Pbrt_bs.array_(a, "get_projects_response", "projects")
      }

      v.projects =
        Array.map(
          json =>
            decode_project_definition(Pbrt_bs.object_(json, "get_projects_response", "projects")),
          a,
        ) |> Array.to_list

    | _ => ()
    } /* Unknown fields are ignored */
  }
  (
    {
      ConfigTypes.projects: v.projects,
    }: ConfigTypes.get_projects_response
  )
}

let rec encode_project_definition = (v: ConfigTypes.project_definition) => {
  let json = Js.Dict.empty()
  Js.Dict.set(json, "name", Js.Json.string(v.ConfigTypes.name))
  Js.Dict.set(json, "repository_regex", Js.Json.string(v.ConfigTypes.repository_regex))
  Js.Dict.set(json, "branch_regex", Js.Json.string(v.ConfigTypes.branch_regex))
  Js.Dict.set(json, "file_regex", Js.Json.string(v.ConfigTypes.file_regex))
  json
}

let rec encode_get_projects_request = (v: ConfigTypes.get_projects_request) => {
  let json = Js.Dict.empty()
  Js.Dict.set(json, "index", Js.Json.string(v.ConfigTypes.index))
  json
}

let rec encode_get_projects_response = (v: ConfigTypes.get_projects_response) => {
  let json = Js.Dict.empty()
  {
    /* projects field */
    let projects': Js.Json.t =
      v.ConfigTypes.projects
      |> Array.of_list
      |> Array.map(v => v |> encode_project_definition |> Js.Json.object_)
      |> Js.Json.array

    Js.Dict.set(json, "projects", projects')
  }
  json
}
