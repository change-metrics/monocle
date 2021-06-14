[@@@ocaml.warning "-27-30-39"]


type who_am_irequest = {
  noop : string;
}

type who_am_iresponse = {
  userid : string;
}

let rec default_who_am_irequest 
  ?noop:((noop:string) = "")
  () : who_am_irequest  = {
  noop;
}

let rec default_who_am_iresponse 
  ?userid:((userid:string) = "")
  () : who_am_iresponse  = {
  userid;
}
