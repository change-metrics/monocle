(** jwt.proto Types *)



(** {2 Types} *)

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


(** {2 Default values} *)

val default_get_magic_jwtrequest : 
  ?token:string ->
  unit ->
  get_magic_jwtrequest
(** [default_get_magic_jwtrequest ()] is the default value for type [get_magic_jwtrequest] *)

val default_unauthorized : 
  ?reason:string ->
  unit ->
  unauthorized
(** [default_unauthorized ()] is the default value for type [unauthorized] *)

val default_success_jwt : 
  ?jwt:string ->
  unit ->
  success_jwt
(** [default_success_jwt ()] is the default value for type [success_jwt] *)

val default_get_magic_jwtresponse : unit -> get_magic_jwtresponse
(** [default_get_magic_jwtresponse ()] is the default value for type [get_magic_jwtresponse] *)
