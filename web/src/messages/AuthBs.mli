(** auth.proto BuckleScript Encoding *)


(** {2 Protobuf JSON Encoding} *)

val encode_who_am_irequest : AuthTypes.who_am_irequest -> Js.Json.t Js.Dict.t
(** [encode_who_am_irequest v dict] encodes [v] int the given JSON [dict] *)

val encode_who_am_iresponse : AuthTypes.who_am_iresponse -> Js.Json.t Js.Dict.t
(** [encode_who_am_iresponse v dict] encodes [v] int the given JSON [dict] *)


(** {2 BS Decoding} *)

val decode_who_am_irequest : Js.Json.t Js.Dict.t -> AuthTypes.who_am_irequest
(** [decode_who_am_irequest decoder] decodes a [who_am_irequest] value from [decoder] *)

val decode_who_am_iresponse : Js.Json.t Js.Dict.t -> AuthTypes.who_am_iresponse
(** [decode_who_am_iresponse decoder] decodes a [who_am_iresponse] value from [decoder] *)
