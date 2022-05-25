(** jwt.proto Types *)



(** {2 Types} *)

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


(** {2 Default values} *)

val default_get_magic_jwtrequest : 
  ?token:string ->
  unit ->
  get_magic_jwtrequest
(** [default_get_magic_jwtrequest ()] is the default value for type [get_magic_jwtrequest] *)

val default_get_magic_jwterror : unit -> get_magic_jwterror
(** [default_get_magic_jwterror ()] is the default value for type [get_magic_jwterror] *)

val default_get_magic_jwtresponse : unit -> get_magic_jwtresponse
(** [default_get_magic_jwtresponse ()] is the default value for type [get_magic_jwtresponse] *)
