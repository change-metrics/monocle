(** change.proto BuckleScript Encoding *)


(** {2 Protobuf JSON Encoding} *)

val encode_ident : ChangeTypes.ident -> Js.Json.t Js.Dict.t
(** [encode_ident v dict] encodes [v] int the given JSON [dict] *)

val encode_changed_file : ChangeTypes.changed_file -> Js.Json.t Js.Dict.t
(** [encode_changed_file v dict] encodes [v] int the given JSON [dict] *)

val encode_changed_file_path : ChangeTypes.changed_file_path -> Js.Json.t Js.Dict.t
(** [encode_changed_file_path v dict] encodes [v] int the given JSON [dict] *)

val encode_commit : ChangeTypes.commit -> Js.Json.t Js.Dict.t
(** [encode_commit v dict] encodes [v] int the given JSON [dict] *)

val encode_change_change_state : ChangeTypes.change_change_state -> string
(** [encode_change_change_state v] returns JSON string*)

val encode_change_optional_merged_by : ChangeTypes.change_optional_merged_by -> Js.Json.t Js.Dict.t
(** [encode_change_optional_merged_by v dict] encodes [v] int the given JSON [dict] *)

val encode_change_optional_merged_at : ChangeTypes.change_optional_merged_at -> Js.Json.t Js.Dict.t
(** [encode_change_optional_merged_at v dict] encodes [v] int the given JSON [dict] *)

val encode_change_optional_closed_at : ChangeTypes.change_optional_closed_at -> Js.Json.t Js.Dict.t
(** [encode_change_optional_closed_at v dict] encodes [v] int the given JSON [dict] *)

val encode_change_optional_duration : ChangeTypes.change_optional_duration -> Js.Json.t Js.Dict.t
(** [encode_change_optional_duration v dict] encodes [v] int the given JSON [dict] *)

val encode_change_optional_self_merged : ChangeTypes.change_optional_self_merged -> Js.Json.t Js.Dict.t
(** [encode_change_optional_self_merged v dict] encodes [v] int the given JSON [dict] *)

val encode_change_optional_merged_commit_sha : ChangeTypes.change_optional_merged_commit_sha -> Js.Json.t Js.Dict.t
(** [encode_change_optional_merged_commit_sha v dict] encodes [v] int the given JSON [dict] *)

val encode_change : ChangeTypes.change -> Js.Json.t Js.Dict.t
(** [encode_change v dict] encodes [v] int the given JSON [dict] *)

val encode_change_reviewed_event : ChangeTypes.change_reviewed_event -> Js.Json.t Js.Dict.t
(** [encode_change_reviewed_event v dict] encodes [v] int the given JSON [dict] *)

val encode_change_event_type : ChangeTypes.change_event_type -> Js.Json.t Js.Dict.t
(** [encode_change_event_type v dict] encodes [v] int the given JSON [dict] *)

val encode_change_event_optional_duration : ChangeTypes.change_event_optional_duration -> Js.Json.t Js.Dict.t
(** [encode_change_event_optional_duration v dict] encodes [v] int the given JSON [dict] *)

val encode_change_event_optional_merged_commit_sha : ChangeTypes.change_event_optional_merged_commit_sha -> Js.Json.t Js.Dict.t
(** [encode_change_event_optional_merged_commit_sha v dict] encodes [v] int the given JSON [dict] *)

val encode_change_event : ChangeTypes.change_event -> Js.Json.t Js.Dict.t
(** [encode_change_event v dict] encodes [v] int the given JSON [dict] *)


(** {2 BS Decoding} *)

val decode_ident : Js.Json.t Js.Dict.t -> ChangeTypes.ident
(** [decode_ident decoder] decodes a [ident] value from [decoder] *)

val decode_changed_file : Js.Json.t Js.Dict.t -> ChangeTypes.changed_file
(** [decode_changed_file decoder] decodes a [changed_file] value from [decoder] *)

val decode_changed_file_path : Js.Json.t Js.Dict.t -> ChangeTypes.changed_file_path
(** [decode_changed_file_path decoder] decodes a [changed_file_path] value from [decoder] *)

val decode_commit : Js.Json.t Js.Dict.t -> ChangeTypes.commit
(** [decode_commit decoder] decodes a [commit] value from [decoder] *)

val decode_change_change_state : Js.Json.t -> ChangeTypes.change_change_state
(** [decode_change_change_state value] decodes a [change_change_state] from a Json value*)

val decode_change_optional_merged_by : Js.Json.t Js.Dict.t -> ChangeTypes.change_optional_merged_by
(** [decode_change_optional_merged_by decoder] decodes a [change_optional_merged_by] value from [decoder] *)

val decode_change_optional_merged_at : Js.Json.t Js.Dict.t -> ChangeTypes.change_optional_merged_at
(** [decode_change_optional_merged_at decoder] decodes a [change_optional_merged_at] value from [decoder] *)

val decode_change_optional_closed_at : Js.Json.t Js.Dict.t -> ChangeTypes.change_optional_closed_at
(** [decode_change_optional_closed_at decoder] decodes a [change_optional_closed_at] value from [decoder] *)

val decode_change_optional_duration : Js.Json.t Js.Dict.t -> ChangeTypes.change_optional_duration
(** [decode_change_optional_duration decoder] decodes a [change_optional_duration] value from [decoder] *)

val decode_change_optional_self_merged : Js.Json.t Js.Dict.t -> ChangeTypes.change_optional_self_merged
(** [decode_change_optional_self_merged decoder] decodes a [change_optional_self_merged] value from [decoder] *)

val decode_change_optional_merged_commit_sha : Js.Json.t Js.Dict.t -> ChangeTypes.change_optional_merged_commit_sha
(** [decode_change_optional_merged_commit_sha decoder] decodes a [change_optional_merged_commit_sha] value from [decoder] *)

val decode_change : Js.Json.t Js.Dict.t -> ChangeTypes.change
(** [decode_change decoder] decodes a [change] value from [decoder] *)

val decode_change_reviewed_event : Js.Json.t Js.Dict.t -> ChangeTypes.change_reviewed_event
(** [decode_change_reviewed_event decoder] decodes a [change_reviewed_event] value from [decoder] *)

val decode_change_event_type : Js.Json.t Js.Dict.t -> ChangeTypes.change_event_type
(** [decode_change_event_type decoder] decodes a [change_event_type] value from [decoder] *)

val decode_change_event_optional_duration : Js.Json.t Js.Dict.t -> ChangeTypes.change_event_optional_duration
(** [decode_change_event_optional_duration decoder] decodes a [change_event_optional_duration] value from [decoder] *)

val decode_change_event_optional_merged_commit_sha : Js.Json.t Js.Dict.t -> ChangeTypes.change_event_optional_merged_commit_sha
(** [decode_change_event_optional_merged_commit_sha decoder] decodes a [change_event_optional_merged_commit_sha] value from [decoder] *)

val decode_change_event : Js.Json.t Js.Dict.t -> ChangeTypes.change_event
(** [decode_change_event decoder] decodes a [change_event] value from [decoder] *)
