(** task_data.proto BuckleScript Encoding *)


(** {2 Protobuf JSON Encoding} *)

val encode_task_data_commit_request : TaskDataTypes.task_data_commit_request -> Js.Json.t Js.Dict.t
(** [encode_task_data_commit_request v dict] encodes [v] int the given JSON [dict] *)

val encode_task_data_commit_error : TaskDataTypes.task_data_commit_error -> string
(** [encode_task_data_commit_error v] returns JSON string*)

val encode_task_data_commit_response : TaskDataTypes.task_data_commit_response -> Js.Json.t Js.Dict.t
(** [encode_task_data_commit_response v dict] encodes [v] int the given JSON [dict] *)

val encode_task_data_get_last_updated_error : TaskDataTypes.task_data_get_last_updated_error -> string
(** [encode_task_data_get_last_updated_error v] returns JSON string*)

val encode_task_data_get_last_updated_request : TaskDataTypes.task_data_get_last_updated_request -> Js.Json.t Js.Dict.t
(** [encode_task_data_get_last_updated_request v dict] encodes [v] int the given JSON [dict] *)

val encode_task_data_get_last_updated_response : TaskDataTypes.task_data_get_last_updated_response -> Js.Json.t Js.Dict.t
(** [encode_task_data_get_last_updated_response v dict] encodes [v] int the given JSON [dict] *)

val encode_task_data : TaskDataTypes.task_data -> Js.Json.t Js.Dict.t
(** [encode_task_data v dict] encodes [v] int the given JSON [dict] *)

val encode_add_request : TaskDataTypes.add_request -> Js.Json.t Js.Dict.t
(** [encode_add_request v dict] encodes [v] int the given JSON [dict] *)

val encode_add_response : TaskDataTypes.add_response -> Js.Json.t Js.Dict.t
(** [encode_add_response v dict] encodes [v] int the given JSON [dict] *)


(** {2 BS Decoding} *)

val decode_task_data_commit_request : Js.Json.t Js.Dict.t -> TaskDataTypes.task_data_commit_request
(** [decode_task_data_commit_request decoder] decodes a [task_data_commit_request] value from [decoder] *)

val decode_task_data_commit_error : Js.Json.t -> TaskDataTypes.task_data_commit_error
(** [decode_task_data_commit_error value] decodes a [task_data_commit_error] from a Json value*)

val decode_task_data_commit_response : Js.Json.t Js.Dict.t -> TaskDataTypes.task_data_commit_response
(** [decode_task_data_commit_response decoder] decodes a [task_data_commit_response] value from [decoder] *)

val decode_task_data_get_last_updated_error : Js.Json.t -> TaskDataTypes.task_data_get_last_updated_error
(** [decode_task_data_get_last_updated_error value] decodes a [task_data_get_last_updated_error] from a Json value*)

val decode_task_data_get_last_updated_request : Js.Json.t Js.Dict.t -> TaskDataTypes.task_data_get_last_updated_request
(** [decode_task_data_get_last_updated_request decoder] decodes a [task_data_get_last_updated_request] value from [decoder] *)

val decode_task_data_get_last_updated_response : Js.Json.t Js.Dict.t -> TaskDataTypes.task_data_get_last_updated_response
(** [decode_task_data_get_last_updated_response decoder] decodes a [task_data_get_last_updated_response] value from [decoder] *)

val decode_task_data : Js.Json.t Js.Dict.t -> TaskDataTypes.task_data
(** [decode_task_data decoder] decodes a [task_data] value from [decoder] *)

val decode_add_request : Js.Json.t Js.Dict.t -> TaskDataTypes.add_request
(** [decode_add_request decoder] decodes a [add_request] value from [decoder] *)

val decode_add_response : Js.Json.t Js.Dict.t -> TaskDataTypes.add_response
(** [decode_add_response decoder] decodes a [add_response] value from [decoder] *)
