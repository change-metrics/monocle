(** search.proto BuckleScript Encoding *)


(** {2 Protobuf JSON Encoding} *)

val encode_search_suggestions_request : SearchTypes.search_suggestions_request -> Js.Json.t Js.Dict.t
(** [encode_search_suggestions_request v dict] encodes [v] int the given JSON [dict] *)

val encode_search_suggestions_response : SearchTypes.search_suggestions_response -> Js.Json.t Js.Dict.t
(** [encode_search_suggestions_response v dict] encodes [v] int the given JSON [dict] *)

val encode_fields_request : SearchTypes.fields_request -> Js.Json.t Js.Dict.t
(** [encode_fields_request v dict] encodes [v] int the given JSON [dict] *)

val encode_field_type : SearchTypes.field_type -> string
(** [encode_field_type v] returns JSON string*)

val encode_field : SearchTypes.field -> Js.Json.t Js.Dict.t
(** [encode_field v dict] encodes [v] int the given JSON [dict] *)

val encode_fields_response : SearchTypes.fields_response -> Js.Json.t Js.Dict.t
(** [encode_fields_response v dict] encodes [v] int the given JSON [dict] *)

val encode_query_error : SearchTypes.query_error -> Js.Json.t Js.Dict.t
(** [encode_query_error v dict] encodes [v] int the given JSON [dict] *)

val encode_order_direction : SearchTypes.order_direction -> string
(** [encode_order_direction v] returns JSON string*)

val encode_order : SearchTypes.order -> Js.Json.t Js.Dict.t
(** [encode_order v dict] encodes [v] int the given JSON [dict] *)

val encode_query_request_query_type : SearchTypes.query_request_query_type -> string
(** [encode_query_request_query_type v] returns JSON string*)

val encode_query_request : SearchTypes.query_request -> Js.Json.t Js.Dict.t
(** [encode_query_request v dict] encodes [v] int the given JSON [dict] *)

val encode_file : SearchTypes.file -> Js.Json.t Js.Dict.t
(** [encode_file v dict] encodes [v] int the given JSON [dict] *)

val encode_commit : SearchTypes.commit -> Js.Json.t Js.Dict.t
(** [encode_commit v dict] encodes [v] int the given JSON [dict] *)

val encode_change_merged_by_m : SearchTypes.change_merged_by_m -> Js.Json.t Js.Dict.t
(** [encode_change_merged_by_m v dict] encodes [v] int the given JSON [dict] *)

val encode_change : SearchTypes.change -> Js.Json.t Js.Dict.t
(** [encode_change v dict] encodes [v] int the given JSON [dict] *)

val encode_changes : SearchTypes.changes -> Js.Json.t Js.Dict.t
(** [encode_changes v dict] encodes [v] int the given JSON [dict] *)

val encode_repo_summary : SearchTypes.repo_summary -> Js.Json.t Js.Dict.t
(** [encode_repo_summary v dict] encodes [v] int the given JSON [dict] *)

val encode_repos_summary : SearchTypes.repos_summary -> Js.Json.t Js.Dict.t
(** [encode_repos_summary v dict] encodes [v] int the given JSON [dict] *)

val encode_query_response : SearchTypes.query_response -> Js.Json.t Js.Dict.t
(** [encode_query_response v dict] encodes [v] int the given JSON [dict] *)

val encode_changes_histos_event : SearchTypes.changes_histos_event -> Js.Json.t Js.Dict.t
(** [encode_changes_histos_event v dict] encodes [v] int the given JSON [dict] *)

val encode_changes_histos : SearchTypes.changes_histos -> Js.Json.t Js.Dict.t
(** [encode_changes_histos v dict] encodes [v] int the given JSON [dict] *)

val encode_changes_lifecycle_event : SearchTypes.changes_lifecycle_event -> Js.Json.t Js.Dict.t
(** [encode_changes_lifecycle_event v dict] encodes [v] int the given JSON [dict] *)

val encode_changes_lifecycle_ratios : SearchTypes.changes_lifecycle_ratios -> Js.Json.t Js.Dict.t
(** [encode_changes_lifecycle_ratios v dict] encodes [v] int the given JSON [dict] *)

val encode_changes_lifecycle : SearchTypes.changes_lifecycle -> Js.Json.t Js.Dict.t
(** [encode_changes_lifecycle v dict] encodes [v] int the given JSON [dict] *)


(** {2 BS Decoding} *)

val decode_search_suggestions_request : Js.Json.t Js.Dict.t -> SearchTypes.search_suggestions_request
(** [decode_search_suggestions_request decoder] decodes a [search_suggestions_request] value from [decoder] *)

val decode_search_suggestions_response : Js.Json.t Js.Dict.t -> SearchTypes.search_suggestions_response
(** [decode_search_suggestions_response decoder] decodes a [search_suggestions_response] value from [decoder] *)

val decode_fields_request : Js.Json.t Js.Dict.t -> SearchTypes.fields_request
(** [decode_fields_request decoder] decodes a [fields_request] value from [decoder] *)

val decode_field_type : Js.Json.t -> SearchTypes.field_type
(** [decode_field_type value] decodes a [field_type] from a Json value*)

val decode_field : Js.Json.t Js.Dict.t -> SearchTypes.field
(** [decode_field decoder] decodes a [field] value from [decoder] *)

val decode_fields_response : Js.Json.t Js.Dict.t -> SearchTypes.fields_response
(** [decode_fields_response decoder] decodes a [fields_response] value from [decoder] *)

val decode_query_error : Js.Json.t Js.Dict.t -> SearchTypes.query_error
(** [decode_query_error decoder] decodes a [query_error] value from [decoder] *)

val decode_order_direction : Js.Json.t -> SearchTypes.order_direction
(** [decode_order_direction value] decodes a [order_direction] from a Json value*)

val decode_order : Js.Json.t Js.Dict.t -> SearchTypes.order
(** [decode_order decoder] decodes a [order] value from [decoder] *)

val decode_query_request_query_type : Js.Json.t -> SearchTypes.query_request_query_type
(** [decode_query_request_query_type value] decodes a [query_request_query_type] from a Json value*)

val decode_query_request : Js.Json.t Js.Dict.t -> SearchTypes.query_request
(** [decode_query_request decoder] decodes a [query_request] value from [decoder] *)

val decode_file : Js.Json.t Js.Dict.t -> SearchTypes.file
(** [decode_file decoder] decodes a [file] value from [decoder] *)

val decode_commit : Js.Json.t Js.Dict.t -> SearchTypes.commit
(** [decode_commit decoder] decodes a [commit] value from [decoder] *)

val decode_change_merged_by_m : Js.Json.t Js.Dict.t -> SearchTypes.change_merged_by_m
(** [decode_change_merged_by_m decoder] decodes a [change_merged_by_m] value from [decoder] *)

val decode_change : Js.Json.t Js.Dict.t -> SearchTypes.change
(** [decode_change decoder] decodes a [change] value from [decoder] *)

val decode_changes : Js.Json.t Js.Dict.t -> SearchTypes.changes
(** [decode_changes decoder] decodes a [changes] value from [decoder] *)

val decode_repo_summary : Js.Json.t Js.Dict.t -> SearchTypes.repo_summary
(** [decode_repo_summary decoder] decodes a [repo_summary] value from [decoder] *)

val decode_repos_summary : Js.Json.t Js.Dict.t -> SearchTypes.repos_summary
(** [decode_repos_summary decoder] decodes a [repos_summary] value from [decoder] *)

val decode_query_response : Js.Json.t Js.Dict.t -> SearchTypes.query_response
(** [decode_query_response decoder] decodes a [query_response] value from [decoder] *)

val decode_changes_histos_event : Js.Json.t Js.Dict.t -> SearchTypes.changes_histos_event
(** [decode_changes_histos_event decoder] decodes a [changes_histos_event] value from [decoder] *)

val decode_changes_histos : Js.Json.t Js.Dict.t -> SearchTypes.changes_histos
(** [decode_changes_histos decoder] decodes a [changes_histos] value from [decoder] *)

val decode_changes_lifecycle_event : Js.Json.t Js.Dict.t -> SearchTypes.changes_lifecycle_event
(** [decode_changes_lifecycle_event decoder] decodes a [changes_lifecycle_event] value from [decoder] *)

val decode_changes_lifecycle_ratios : Js.Json.t Js.Dict.t -> SearchTypes.changes_lifecycle_ratios
(** [decode_changes_lifecycle_ratios decoder] decodes a [changes_lifecycle_ratios] value from [decoder] *)

val decode_changes_lifecycle : Js.Json.t Js.Dict.t -> SearchTypes.changes_lifecycle
(** [decode_changes_lifecycle decoder] decodes a [changes_lifecycle] value from [decoder] *)
