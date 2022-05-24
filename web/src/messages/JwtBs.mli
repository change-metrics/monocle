(** jwt.proto BuckleScript Encoding *)


(** {2 Protobuf JSON Encoding} *)

val encode_get_magic_jwtrequest : JwtTypes.get_magic_jwtrequest -> Js.Json.t Js.Dict.t
(** [encode_get_magic_jwtrequest v dict] encodes [v] int the given JSON [dict] *)

val encode_unauthorized : JwtTypes.unauthorized -> Js.Json.t Js.Dict.t
(** [encode_unauthorized v dict] encodes [v] int the given JSON [dict] *)

val encode_success_jwt : JwtTypes.success_jwt -> Js.Json.t Js.Dict.t
(** [encode_success_jwt v dict] encodes [v] int the given JSON [dict] *)

val encode_get_magic_jwtresponse : JwtTypes.get_magic_jwtresponse -> Js.Json.t Js.Dict.t
(** [encode_get_magic_jwtresponse v dict] encodes [v] int the given JSON [dict] *)


(** {2 BS Decoding} *)

val decode_get_magic_jwtrequest : Js.Json.t Js.Dict.t -> JwtTypes.get_magic_jwtrequest
(** [decode_get_magic_jwtrequest decoder] decodes a [get_magic_jwtrequest] value from [decoder] *)

val decode_unauthorized : Js.Json.t Js.Dict.t -> JwtTypes.unauthorized
(** [decode_unauthorized decoder] decodes a [unauthorized] value from [decoder] *)

val decode_success_jwt : Js.Json.t Js.Dict.t -> JwtTypes.success_jwt
(** [decode_success_jwt decoder] decodes a [success_jwt] value from [decoder] *)

val decode_get_magic_jwtresponse : Js.Json.t Js.Dict.t -> JwtTypes.get_magic_jwtresponse
(** [decode_get_magic_jwtresponse decoder] decodes a [get_magic_jwtresponse] value from [decoder] *)
