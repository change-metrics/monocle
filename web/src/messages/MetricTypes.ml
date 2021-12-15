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
