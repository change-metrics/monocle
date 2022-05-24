[@@@ocaml.warning "-27-30-39"]


type get_magic_jwtrequest = {
  token : string;
}

type unauthorized = {
  reason : string;
}

type success_jwt = {
  jwt : string;
}

type get_magic_jwtresponse =
  | Reason of unauthorized
  | Success_jwt of success_jwt

let rec default_get_magic_jwtrequest 
  ?token:((token:string) = "")
  () : get_magic_jwtrequest  = {
  token;
}

let rec default_unauthorized 
  ?reason:((reason:string) = "")
  () : unauthorized  = {
  reason;
}

let rec default_success_jwt 
  ?jwt:((jwt:string) = "")
  () : success_jwt  = {
  jwt;
}

let rec default_get_magic_jwtresponse () : get_magic_jwtresponse = Reason (default_unauthorized ())
