(** auth.proto Types *)



(** {2 Types} *)

type who_am_irequest = {
  noop : string;
}

type who_am_iresponse = {
  userid : string;
}


(** {2 Default values} *)

val default_who_am_irequest : 
  ?noop:string ->
  unit ->
  who_am_irequest
(** [default_who_am_irequest ()] is the default value for type [who_am_irequest] *)

val default_who_am_iresponse : 
  ?userid:string ->
  unit ->
  who_am_iresponse
(** [default_who_am_iresponse ()] is the default value for type [who_am_iresponse] *)
