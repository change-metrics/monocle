(** user_group.proto BuckleScript Encoding *)


(** {2 Protobuf JSON Encoding} *)

val encode_group_definition : UserGroupTypes.group_definition -> Js.Json.t Js.Dict.t
(** [encode_group_definition v dict] encodes [v] int the given JSON [dict] *)

val encode_list_request : UserGroupTypes.list_request -> Js.Json.t Js.Dict.t
(** [encode_list_request v dict] encodes [v] int the given JSON [dict] *)

val encode_list_response : UserGroupTypes.list_response -> Js.Json.t Js.Dict.t
(** [encode_list_response v dict] encodes [v] int the given JSON [dict] *)


(** {2 BS Decoding} *)

val decode_group_definition : Js.Json.t Js.Dict.t -> UserGroupTypes.group_definition
(** [decode_group_definition decoder] decodes a [group_definition] value from [decoder] *)

val decode_list_request : Js.Json.t Js.Dict.t -> UserGroupTypes.list_request
(** [decode_list_request decoder] decodes a [list_request] value from [decoder] *)

val decode_list_response : Js.Json.t Js.Dict.t -> UserGroupTypes.list_response
(** [decode_list_response decoder] decodes a [list_response] value from [decoder] *)
