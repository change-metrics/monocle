@@ocaml.warning("-27-30-39")

type task_data_commit_request = {
  index: string,
  crawler: string,
  apikey: string,
  timestamp: string,
}

type task_data_commit_error =
  | Unknown_index
  | Unknown_crawler
  | Unknown_api_key
  | Commit_date_inferior_than_previous

type task_data_commit_response =
  | Error(task_data_commit_error)
  | Timestamp(string)

let rec default_task_data_commit_request = (
  ~index: string="",
  ~crawler: string="",
  ~apikey: string="",
  ~timestamp: string="",
  (),
): task_data_commit_request => {
  index: index,
  crawler: crawler,
  apikey: apikey,
  timestamp: timestamp,
}

let rec default_task_data_commit_error = (): task_data_commit_error => Unknown_index

let rec default_task_data_commit_response = (): task_data_commit_response => Error(
  default_task_data_commit_error(),
)
