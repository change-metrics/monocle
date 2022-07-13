[@@@ocaml.warning "-27-30-39"]


type metric_info = {
  name : string;
  description : string;
  long_description : string;
  metric : string;
}

type list_request = {
  void : string;
}

type list_response = {
  metrics : metric_info list;
}

type trend = {
  interval : int32;
}

type get_request_options =
  | Trend of trend

and get_request = {
  index : string;
  username : string;
  query : string;
  metric : string;
  options : get_request_options;
}

type histo = {
  date : string;
  count : int32;
}

type histo_stat = {
  histo : histo list;
}

type get_response =
  | Error of string
  | Float_value of float
  | Int_value of int32
  | Histo_value of histo_stat

let rec default_metric_info 
  ?name:((name:string) = "")
  ?description:((description:string) = "")
  ?long_description:((long_description:string) = "")
  ?metric:((metric:string) = "")
  () : metric_info  = {
  name;
  description;
  long_description;
  metric;
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

let rec default_trend 
  ?interval:((interval:int32) = 0l)
  () : trend  = {
  interval;
}

let rec default_get_request_options () : get_request_options = Trend (default_trend ())

and default_get_request 
  ?index:((index:string) = "")
  ?username:((username:string) = "")
  ?query:((query:string) = "")
  ?metric:((metric:string) = "")
  ?options:((options:get_request_options) = Trend (default_trend ()))
  () : get_request  = {
  index;
  username;
  query;
  metric;
  options;
}

let rec default_histo 
  ?date:((date:string) = "")
  ?count:((count:int32) = 0l)
  () : histo  = {
  date;
  count;
}

let rec default_histo_stat 
  ?histo:((histo:histo list) = [])
  () : histo_stat  = {
  histo;
}

let rec default_get_response () : get_response = Error ("")
