(** auth.proto BuckleScript Encoding *)


(** {2 Protobuf JSON Encoding} *)

val encode_get_magic_jwt_request : AuthTypes.get_magic_jwt_request -> Js.Json.t Js.Dict.t
(** [encode_get_magic_jwt_request v dict] encodes [v] int the given JSON [dict] *)

val encode_get_magic_jwt_error : AuthTypes.get_magic_jwt_error -> string
(** [encode_get_magic_jwt_error v] returns JSON string*)

val encode_get_magic_jwt_response : AuthTypes.get_magic_jwt_response -> Js.Json.t Js.Dict.t
(** [encode_get_magic_jwt_response v dict] encodes [v] int the given JSON [dict] *)

val encode_who_ami_request : AuthTypes.who_ami_request -> Js.Json.t Js.Dict.t
(** [encode_who_ami_request v dict] encodes [v] int the given JSON [dict] *)

val encode_who_ami_error : AuthTypes.who_ami_error -> string
(** [encode_who_ami_error v] returns JSON string*)

val encode_who_ami_response : AuthTypes.who_ami_response -> Js.Json.t Js.Dict.t
(** [encode_who_ami_response v dict] encodes [v] int the given JSON [dict] *)


(** {2 BS Decoding} *)

val decode_get_magic_jwt_request : Js.Json.t Js.Dict.t -> AuthTypes.get_magic_jwt_request
(** [decode_get_magic_jwt_request decoder] decodes a [get_magic_jwt_request] value from [decoder] *)

val decode_get_magic_jwt_error : Js.Json.t -> AuthTypes.get_magic_jwt_error
(** [decode_get_magic_jwt_error value] decodes a [get_magic_jwt_error] from a Json value*)

val decode_get_magic_jwt_response : Js.Json.t Js.Dict.t -> AuthTypes.get_magic_jwt_response
(** [decode_get_magic_jwt_response decoder] decodes a [get_magic_jwt_response] value from [decoder] *)

val decode_who_ami_request : Js.Json.t Js.Dict.t -> AuthTypes.who_ami_request
(** [decode_who_ami_request decoder] decodes a [who_ami_request] value from [decoder] *)

val decode_who_ami_error : Js.Json.t -> AuthTypes.who_ami_error
(** [decode_who_ami_error value] decodes a [who_ami_error] from a Json value*)

val decode_who_ami_response : Js.Json.t Js.Dict.t -> AuthTypes.who_ami_response
(** [decode_who_ami_response decoder] decodes a [who_ami_response] value from [decoder] *)
