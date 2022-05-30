[@@@ocaml.warning "-27-30-39"]


type get_magic_jwt_request = {
  token : string;
}

type get_magic_jwt_error =
  | Invalid_admin_token 
  | Magic_token_disabled 
  | Magic_token_create_error 

type get_magic_jwt_response =
  | Error of get_magic_jwt_error
  | Jwt of string

type who_ami_request = {
  void : string;
}

type who_ami_error =
  | Un_authorized 

type who_ami_response =
  | Error of who_ami_error
  | Uid of string

let rec default_get_magic_jwt_request 
  ?token:((token:string) = "")
  () : get_magic_jwt_request  = {
  token;
}

let rec default_get_magic_jwt_error () = (Invalid_admin_token:get_magic_jwt_error)

let rec default_get_magic_jwt_response () : get_magic_jwt_response = Error (default_get_magic_jwt_error ())

let rec default_who_ami_request 
  ?void:((void:string) = "")
  () : who_ami_request  = {
  void;
}

let rec default_who_ami_error () = (Un_authorized:who_ami_error)

let rec default_who_ami_response () : who_ami_response = Error (default_who_ami_error ())
