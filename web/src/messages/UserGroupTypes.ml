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

type review_histo = {
  date : int64;
  count : int32;
}

type group_stat = {
  change_review_ratio : float;
  author_review_ratio : float;
  commit_histo : review_histo list;
  review_histo : review_histo list;
}

type user_stat = {
  name : string;
  stat : group_stat option;
}

type get_request = {
  index : string;
  name : string;
  query : string;
}

type get_response = {
  all : group_stat option;
  users : user_stat list;
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

let rec default_review_histo 
  ?date:((date:int64) = 0L)
  ?count:((count:int32) = 0l)
  () : review_histo  = {
  date;
  count;
}

let rec default_group_stat 
  ?change_review_ratio:((change_review_ratio:float) = 0.)
  ?author_review_ratio:((author_review_ratio:float) = 0.)
  ?commit_histo:((commit_histo:review_histo list) = [])
  ?review_histo:((review_histo:review_histo list) = [])
  () : group_stat  = {
  change_review_ratio;
  author_review_ratio;
  commit_histo;
  review_histo;
}

let rec default_user_stat 
  ?name:((name:string) = "")
  ?stat:((stat:group_stat option) = None)
  () : user_stat  = {
  name;
  stat;
}

let rec default_get_request 
  ?index:((index:string) = "")
  ?name:((name:string) = "")
  ?query:((query:string) = "")
  () : get_request  = {
  index;
  name;
  query;
}

let rec default_get_response 
  ?all:((all:group_stat option) = None)
  ?users:((users:user_stat list) = [])
  () : get_response  = {
  all;
  users;
}
