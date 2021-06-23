[@@@ocaml.warning "-27-30-39"]


type group_definition = {
  name : string;
  members : int32;
}

type list_request = {
  index : string;
}

type list_response = {
  items : group_definition list;
}

let rec default_group_definition 
  ?name:((name:string) = "")
  ?members:((members:int32) = 0l)
  () : group_definition  = {
  name;
  members;
}

let rec default_list_request 
  ?index:((index:string) = "")
  () : list_request  = {
  index;
}

let rec default_list_response 
  ?items:((items:group_definition list) = [])
  () : list_response  = {
  items;
}
