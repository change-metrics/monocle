(** auth.proto Types *)



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

type who_am_irequest = {
  void : string;
}

type who_am_ierror =
  | Un_authorized 

type who_am_iresponse =
  | Error of who_am_ierror
  | Uid of string


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

val default_who_am_irequest : 
  ?void:string ->
  unit ->
  who_am_irequest
(** [default_who_am_irequest ()] is the default value for type [who_am_irequest] *)

val default_who_am_ierror : unit -> who_am_ierror
(** [default_who_am_ierror ()] is the default value for type [who_am_ierror] *)

val default_who_am_iresponse : unit -> who_am_iresponse
(** [default_who_am_iresponse ()] is the default value for type [who_am_iresponse] *)
