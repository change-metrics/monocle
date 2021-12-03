(** login.proto BuckleScript Encoding *)


(** {2 Protobuf JSON Encoding} *)

val encode_login_validation_request : LoginTypes.login_validation_request -> Js.Json.t Js.Dict.t
(** [encode_login_validation_request v dict] encodes [v] int the given JSON [dict] *)

val encode_login_validation_response_validation_result : LoginTypes.login_validation_response_validation_result -> string
(** [encode_login_validation_response_validation_result v] returns JSON string*)

val encode_login_validation_response : LoginTypes.login_validation_response -> Js.Json.t Js.Dict.t
(** [encode_login_validation_response v dict] encodes [v] int the given JSON [dict] *)


(** {2 BS Decoding} *)

val decode_login_validation_request : Js.Json.t Js.Dict.t -> LoginTypes.login_validation_request
(** [decode_login_validation_request decoder] decodes a [login_validation_request] value from [decoder] *)

val decode_login_validation_response_validation_result : Js.Json.t -> LoginTypes.login_validation_response_validation_result
(** [decode_login_validation_response_validation_result value] decodes a [login_validation_response_validation_result] from a Json value*)

val decode_login_validation_response : Js.Json.t Js.Dict.t -> LoginTypes.login_validation_response
(** [decode_login_validation_response decoder] decodes a [login_validation_response] value from [decoder] *)
