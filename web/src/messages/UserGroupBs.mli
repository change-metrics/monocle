(** user_group.proto BuckleScript Encoding *)


(** {2 Protobuf JSON Encoding} *)

val encode_group_definition : UserGroupTypes.group_definition -> Js.Json.t Js.Dict.t
(** [encode_group_definition v dict] encodes [v] int the given JSON [dict] *)

val encode_list_request : UserGroupTypes.list_request -> Js.Json.t Js.Dict.t
(** [encode_list_request v dict] encodes [v] int the given JSON [dict] *)

val encode_list_response : UserGroupTypes.list_response -> Js.Json.t Js.Dict.t
(** [encode_list_response v dict] encodes [v] int the given JSON [dict] *)

val encode_review_histo : UserGroupTypes.review_histo -> Js.Json.t Js.Dict.t
(** [encode_review_histo v dict] encodes [v] int the given JSON [dict] *)

val encode_group_stat : UserGroupTypes.group_stat -> Js.Json.t Js.Dict.t
(** [encode_group_stat v dict] encodes [v] int the given JSON [dict] *)

val encode_user_stat : UserGroupTypes.user_stat -> Js.Json.t Js.Dict.t
(** [encode_user_stat v dict] encodes [v] int the given JSON [dict] *)

val encode_get_request : UserGroupTypes.get_request -> Js.Json.t Js.Dict.t
(** [encode_get_request v dict] encodes [v] int the given JSON [dict] *)

val encode_get_response : UserGroupTypes.get_response -> Js.Json.t Js.Dict.t
(** [encode_get_response v dict] encodes [v] int the given JSON [dict] *)


(** {2 BS Decoding} *)

val decode_group_definition : Js.Json.t Js.Dict.t -> UserGroupTypes.group_definition
(** [decode_group_definition decoder] decodes a [group_definition] value from [decoder] *)

val decode_list_request : Js.Json.t Js.Dict.t -> UserGroupTypes.list_request
(** [decode_list_request decoder] decodes a [list_request] value from [decoder] *)

val decode_list_response : Js.Json.t Js.Dict.t -> UserGroupTypes.list_response
(** [decode_list_response decoder] decodes a [list_response] value from [decoder] *)

val decode_review_histo : Js.Json.t Js.Dict.t -> UserGroupTypes.review_histo
(** [decode_review_histo decoder] decodes a [review_histo] value from [decoder] *)

val decode_group_stat : Js.Json.t Js.Dict.t -> UserGroupTypes.group_stat
(** [decode_group_stat decoder] decodes a [group_stat] value from [decoder] *)

val decode_user_stat : Js.Json.t Js.Dict.t -> UserGroupTypes.user_stat
(** [decode_user_stat decoder] decodes a [user_stat] value from [decoder] *)

val decode_get_request : Js.Json.t Js.Dict.t -> UserGroupTypes.get_request
(** [decode_get_request decoder] decodes a [get_request] value from [decoder] *)

val decode_get_response : Js.Json.t Js.Dict.t -> UserGroupTypes.get_response
(** [decode_get_response decoder] decodes a [get_response] value from [decoder] *)
