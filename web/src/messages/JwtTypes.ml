[@@@ocaml.warning "-27-30-39"]


type get_magic_jwtrequest = {
  token : string;
}

type get_magic_jwterror =
  | Invalid_admin_token 
  | Magic_token_disabled 
  | Magic_token_create_error 

type get_magic_jwtresponse =
  | Error of get_magic_jwterror
  | Jwt of string

let rec default_get_magic_jwtrequest 
  ?token:((token:string) = "")
  () : get_magic_jwtrequest  = {
  token;
}

let rec default_get_magic_jwterror () = (Invalid_admin_token:get_magic_jwterror)

let rec default_get_magic_jwtresponse () : get_magic_jwtresponse = Error (default_get_magic_jwterror ())
