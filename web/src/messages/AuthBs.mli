(** auth.proto BuckleScript Encoding *)


(** {2 Protobuf JSON Encoding} *)

val encode_get_magic_jwtrequest : AuthTypes.get_magic_jwtrequest -> Js.Json.t Js.Dict.t
(** [encode_get_magic_jwtrequest v dict] encodes [v] int the given JSON [dict] *)

val encode_get_magic_jwterror : AuthTypes.get_magic_jwterror -> string
(** [encode_get_magic_jwterror v] returns JSON string*)

val encode_get_magic_jwtresponse : AuthTypes.get_magic_jwtresponse -> Js.Json.t Js.Dict.t
(** [encode_get_magic_jwtresponse v dict] encodes [v] int the given JSON [dict] *)

val encode_who_am_irequest : AuthTypes.who_am_irequest -> Js.Json.t Js.Dict.t
(** [encode_who_am_irequest v dict] encodes [v] int the given JSON [dict] *)

val encode_who_am_ierror : AuthTypes.who_am_ierror -> string
(** [encode_who_am_ierror v] returns JSON string*)

val encode_who_am_iresponse : AuthTypes.who_am_iresponse -> Js.Json.t Js.Dict.t
(** [encode_who_am_iresponse v dict] encodes [v] int the given JSON [dict] *)


(** {2 BS Decoding} *)

val decode_get_magic_jwtrequest : Js.Json.t Js.Dict.t -> AuthTypes.get_magic_jwtrequest
(** [decode_get_magic_jwtrequest decoder] decodes a [get_magic_jwtrequest] value from [decoder] *)

val decode_get_magic_jwterror : Js.Json.t -> AuthTypes.get_magic_jwterror
(** [decode_get_magic_jwterror value] decodes a [get_magic_jwterror] from a Json value*)

val decode_get_magic_jwtresponse : Js.Json.t Js.Dict.t -> AuthTypes.get_magic_jwtresponse
(** [decode_get_magic_jwtresponse decoder] decodes a [get_magic_jwtresponse] value from [decoder] *)

val decode_who_am_irequest : Js.Json.t Js.Dict.t -> AuthTypes.who_am_irequest
(** [decode_who_am_irequest decoder] decodes a [who_am_irequest] value from [decoder] *)

val decode_who_am_ierror : Js.Json.t -> AuthTypes.who_am_ierror
(** [decode_who_am_ierror value] decodes a [who_am_ierror] from a Json value*)

val decode_who_am_iresponse : Js.Json.t Js.Dict.t -> AuthTypes.who_am_iresponse
(** [decode_who_am_iresponse decoder] decodes a [who_am_iresponse] value from [decoder] *)
