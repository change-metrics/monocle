(** auth.proto Types *)



(** {2 Types} *)

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


(** {2 Default values} *)

val default_get_magic_jwt_request : 
  ?token:string ->
  unit ->
  get_magic_jwt_request
(** [default_get_magic_jwt_request ()] is the default value for type [get_magic_jwt_request] *)

val default_get_magic_jwt_error : unit -> get_magic_jwt_error
(** [default_get_magic_jwt_error ()] is the default value for type [get_magic_jwt_error] *)

val default_get_magic_jwt_response : unit -> get_magic_jwt_response
(** [default_get_magic_jwt_response ()] is the default value for type [get_magic_jwt_response] *)

val default_who_ami_request : 
  ?void:string ->
  unit ->
  who_ami_request
(** [default_who_ami_request ()] is the default value for type [who_ami_request] *)

val default_who_ami_error : unit -> who_ami_error
(** [default_who_ami_error ()] is the default value for type [who_ami_error] *)

val default_who_ami_response : unit -> who_ami_response
(** [default_who_ami_response ()] is the default value for type [who_ami_response] *)
