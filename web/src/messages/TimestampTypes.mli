(** timestamp.proto Types *)



(** {2 Types} *)

type timestamp = {
  seconds : int64;
  nanos : int32;
}


(** {2 Default values} *)

val default_timestamp : 
  ?seconds:int64 ->
  ?nanos:int32 ->
  unit ->
  timestamp
(** [default_timestamp ()] is the default value for type [timestamp] *)
