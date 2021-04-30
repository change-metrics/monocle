(** task_data.proto BuckleScript Encoding *)


(** {2 Protobuf JSON Encoding} *)

val encode_task_data_commit_request : TaskDataTypes.task_data_commit_request -> Js.Json.t Js.Dict.t
(** [encode_task_data_commit_request v dict] encodes [v] int the given JSON [dict] *)

val encode_task_data_commit_error : TaskDataTypes.task_data_commit_error -> string
(** [encode_task_data_commit_error v] returns JSON string*)

val encode_task_data_commit_response : TaskDataTypes.task_data_commit_response -> Js.Json.t Js.Dict.t
(** [encode_task_data_commit_response v dict] encodes [v] int the given JSON [dict] *)


(** {2 BS Decoding} *)

val decode_task_data_commit_request : Js.Json.t Js.Dict.t -> TaskDataTypes.task_data_commit_request
(** [decode_task_data_commit_request decoder] decodes a [task_data_commit_request] value from [decoder] *)

val decode_task_data_commit_error : Js.Json.t -> TaskDataTypes.task_data_commit_error
(** [decode_task_data_commit_error value] decodes a [task_data_commit_error] from a Json value*)

val decode_task_data_commit_response : Js.Json.t Js.Dict.t -> TaskDataTypes.task_data_commit_response
(** [decode_task_data_commit_response decoder] decodes a [task_data_commit_response] value from [decoder] *)
