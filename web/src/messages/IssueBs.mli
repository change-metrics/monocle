(** issue.proto BuckleScript Encoding *)


(** {2 Protobuf JSON Encoding} *)

val encode_ident : IssueTypes.ident -> Js.Json.t Js.Dict.t
(** [encode_ident v dict] encodes [v] int the given JSON [dict] *)

val encode_issue_optional_closed_at : IssueTypes.issue_optional_closed_at -> Js.Json.t Js.Dict.t
(** [encode_issue_optional_closed_at v dict] encodes [v] int the given JSON [dict] *)

val encode_issue : IssueTypes.issue -> Js.Json.t Js.Dict.t
(** [encode_issue v dict] encodes [v] int the given JSON [dict] *)

val encode_issue_commented_event : IssueTypes.issue_commented_event -> Js.Json.t Js.Dict.t
(** [encode_issue_commented_event v dict] encodes [v] int the given JSON [dict] *)

val encode_issue_event_type : IssueTypes.issue_event_type -> Js.Json.t Js.Dict.t
(** [encode_issue_event_type v dict] encodes [v] int the given JSON [dict] *)

val encode_issue_event : IssueTypes.issue_event -> Js.Json.t Js.Dict.t
(** [encode_issue_event v dict] encodes [v] int the given JSON [dict] *)


(** {2 BS Decoding} *)

val decode_ident : Js.Json.t Js.Dict.t -> IssueTypes.ident
(** [decode_ident decoder] decodes a [ident] value from [decoder] *)

val decode_issue_optional_closed_at : Js.Json.t Js.Dict.t -> IssueTypes.issue_optional_closed_at
(** [decode_issue_optional_closed_at decoder] decodes a [issue_optional_closed_at] value from [decoder] *)

val decode_issue : Js.Json.t Js.Dict.t -> IssueTypes.issue
(** [decode_issue decoder] decodes a [issue] value from [decoder] *)

val decode_issue_commented_event : Js.Json.t Js.Dict.t -> IssueTypes.issue_commented_event
(** [decode_issue_commented_event decoder] decodes a [issue_commented_event] value from [decoder] *)

val decode_issue_event_type : Js.Json.t Js.Dict.t -> IssueTypes.issue_event_type
(** [decode_issue_event_type decoder] decodes a [issue_event_type] value from [decoder] *)

val decode_issue_event : Js.Json.t Js.Dict.t -> IssueTypes.issue_event
(** [decode_issue_event decoder] decodes a [issue_event] value from [decoder] *)
