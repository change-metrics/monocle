[@@@ocaml.warning "-27-30-39"]


type metric_info = {
  name : string;
  description : string;
  long_description : string;
}

type list_request = {
  void : string;
}

type list_response = {
  metrics : metric_info list;
}

type get_request = {
  index : string;
  username : string;
  query : string;
  metric : string;
}

type get_response =
  | Error of string
  | Float_value of float

let rec default_metric_info 
  ?name:((name:string) = "")
  ?description:((description:string) = "")
  ?long_description:((long_description:string) = "")
  () : metric_info  = {
  name;
  description;
  long_description;
}

let rec default_list_request 
  ?void:((void:string) = "")
  () : list_request  = {
  void;
}

let rec default_list_response 
  ?metrics:((metrics:metric_info list) = [])
  () : list_response  = {
  metrics;
}

let rec default_get_request 
  ?index:((index:string) = "")
  ?username:((username:string) = "")
  ?query:((query:string) = "")
  ?metric:((metric:string) = "")
  () : get_request  = {
  index;
  username;
  query;
  metric;
}

let rec default_get_response () : get_response = Error ("")
