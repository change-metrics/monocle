(** jwt.proto BuckleScript Encoding *)


(** {2 Protobuf JSON Encoding} *)

val encode_get_magic_jwtrequest : JwtTypes.get_magic_jwtrequest -> Js.Json.t Js.Dict.t
(** [encode_get_magic_jwtrequest v dict] encodes [v] int the given JSON [dict] *)

val encode_get_magic_jwterror : JwtTypes.get_magic_jwterror -> string
(** [encode_get_magic_jwterror v] returns JSON string*)

val encode_get_magic_jwtresponse : JwtTypes.get_magic_jwtresponse -> Js.Json.t Js.Dict.t
(** [encode_get_magic_jwtresponse v dict] encodes [v] int the given JSON [dict] *)


(** {2 BS Decoding} *)

val decode_get_magic_jwtrequest : Js.Json.t Js.Dict.t -> JwtTypes.get_magic_jwtrequest
(** [decode_get_magic_jwtrequest decoder] decodes a [get_magic_jwtrequest] value from [decoder] *)

val decode_get_magic_jwterror : Js.Json.t -> JwtTypes.get_magic_jwterror
(** [decode_get_magic_jwterror value] decodes a [get_magic_jwterror] from a Json value*)

val decode_get_magic_jwtresponse : Js.Json.t Js.Dict.t -> JwtTypes.get_magic_jwtresponse
(** [decode_get_magic_jwtresponse decoder] decodes a [get_magic_jwtresponse] value from [decoder] *)
