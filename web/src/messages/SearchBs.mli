(** search.proto BuckleScript Encoding *)


(** {2 Protobuf JSON Encoding} *)

val encode_task_data : SearchTypes.task_data -> Js.Json.t Js.Dict.t
(** [encode_task_data v dict] encodes [v] int the given JSON [dict] *)

val encode_suggestions_request : SearchTypes.suggestions_request -> Js.Json.t Js.Dict.t
(** [encode_suggestions_request v dict] encodes [v] int the given JSON [dict] *)

val encode_suggestions_response : SearchTypes.suggestions_response -> Js.Json.t Js.Dict.t
(** [encode_suggestions_response v dict] encodes [v] int the given JSON [dict] *)

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

val encode_check_request : SearchTypes.check_request -> Js.Json.t Js.Dict.t
(** [encode_check_request v dict] encodes [v] int the given JSON [dict] *)

val encode_check_response : SearchTypes.check_response -> Js.Json.t Js.Dict.t
(** [encode_check_response v dict] encodes [v] int the given JSON [dict] *)

val encode_author : SearchTypes.author -> Js.Json.t Js.Dict.t
(** [encode_author v dict] encodes [v] int the given JSON [dict] *)

val encode_author_request : SearchTypes.author_request -> Js.Json.t Js.Dict.t
(** [encode_author_request v dict] encodes [v] int the given JSON [dict] *)

val encode_author_response : SearchTypes.author_response -> Js.Json.t Js.Dict.t
(** [encode_author_response v dict] encodes [v] int the given JSON [dict] *)

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

val encode_ratio : SearchTypes.ratio -> Js.Json.t Js.Dict.t
(** [encode_ratio v dict] encodes [v] int the given JSON [dict] *)

val encode_change_event : SearchTypes.change_event -> Js.Json.t Js.Dict.t
(** [encode_change_event v dict] encodes [v] int the given JSON [dict] *)

val encode_change_and_events : SearchTypes.change_and_events -> Js.Json.t Js.Dict.t
(** [encode_change_and_events v dict] encodes [v] int the given JSON [dict] *)

val encode_review_count : SearchTypes.review_count -> Js.Json.t Js.Dict.t
(** [encode_review_count v dict] encodes [v] int the given JSON [dict] *)

val encode_review_stats : SearchTypes.review_stats -> Js.Json.t Js.Dict.t
(** [encode_review_stats v dict] encodes [v] int the given JSON [dict] *)

val encode_activity_stats : SearchTypes.activity_stats -> Js.Json.t Js.Dict.t
(** [encode_activity_stats v dict] encodes [v] int the given JSON [dict] *)

val encode_repo_summary : SearchTypes.repo_summary -> Js.Json.t Js.Dict.t
(** [encode_repo_summary v dict] encodes [v] int the given JSON [dict] *)

val encode_repos_summary : SearchTypes.repos_summary -> Js.Json.t Js.Dict.t
(** [encode_repos_summary v dict] encodes [v] int the given JSON [dict] *)

val encode_term_count : SearchTypes.term_count -> Js.Json.t Js.Dict.t
(** [encode_term_count v dict] encodes [v] int the given JSON [dict] *)

val encode_terms_count : SearchTypes.terms_count -> Js.Json.t Js.Dict.t
(** [encode_terms_count v dict] encodes [v] int the given JSON [dict] *)

val encode_author_peer : SearchTypes.author_peer -> Js.Json.t Js.Dict.t
(** [encode_author_peer v dict] encodes [v] int the given JSON [dict] *)

val encode_authors_peers : SearchTypes.authors_peers -> Js.Json.t Js.Dict.t
(** [encode_authors_peers v dict] encodes [v] int the given JSON [dict] *)

val encode_lifecycle_stats : SearchTypes.lifecycle_stats -> Js.Json.t Js.Dict.t
(** [encode_lifecycle_stats v dict] encodes [v] int the given JSON [dict] *)

val encode_changes_tops : SearchTypes.changes_tops -> Js.Json.t Js.Dict.t
(** [encode_changes_tops v dict] encodes [v] int the given JSON [dict] *)

val encode_query_response : SearchTypes.query_response -> Js.Json.t Js.Dict.t
(** [encode_query_response v dict] encodes [v] int the given JSON [dict] *)


(** {2 BS Decoding} *)

val decode_task_data : Js.Json.t Js.Dict.t -> SearchTypes.task_data
(** [decode_task_data decoder] decodes a [task_data] value from [decoder] *)

val decode_suggestions_request : Js.Json.t Js.Dict.t -> SearchTypes.suggestions_request
(** [decode_suggestions_request decoder] decodes a [suggestions_request] value from [decoder] *)

val decode_suggestions_response : Js.Json.t Js.Dict.t -> SearchTypes.suggestions_response
(** [decode_suggestions_response decoder] decodes a [suggestions_response] value from [decoder] *)

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

val decode_check_request : Js.Json.t Js.Dict.t -> SearchTypes.check_request
(** [decode_check_request decoder] decodes a [check_request] value from [decoder] *)

val decode_check_response : Js.Json.t Js.Dict.t -> SearchTypes.check_response
(** [decode_check_response decoder] decodes a [check_response] value from [decoder] *)

val decode_author : Js.Json.t Js.Dict.t -> SearchTypes.author
(** [decode_author decoder] decodes a [author] value from [decoder] *)

val decode_author_request : Js.Json.t Js.Dict.t -> SearchTypes.author_request
(** [decode_author_request decoder] decodes a [author_request] value from [decoder] *)

val decode_author_response : Js.Json.t Js.Dict.t -> SearchTypes.author_response
(** [decode_author_response decoder] decodes a [author_response] value from [decoder] *)

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

val decode_ratio : Js.Json.t Js.Dict.t -> SearchTypes.ratio
(** [decode_ratio decoder] decodes a [ratio] value from [decoder] *)

val decode_change_event : Js.Json.t Js.Dict.t -> SearchTypes.change_event
(** [decode_change_event decoder] decodes a [change_event] value from [decoder] *)

val decode_change_and_events : Js.Json.t Js.Dict.t -> SearchTypes.change_and_events
(** [decode_change_and_events decoder] decodes a [change_and_events] value from [decoder] *)

val decode_review_count : Js.Json.t Js.Dict.t -> SearchTypes.review_count
(** [decode_review_count decoder] decodes a [review_count] value from [decoder] *)

val decode_review_stats : Js.Json.t Js.Dict.t -> SearchTypes.review_stats
(** [decode_review_stats decoder] decodes a [review_stats] value from [decoder] *)

val decode_activity_stats : Js.Json.t Js.Dict.t -> SearchTypes.activity_stats
(** [decode_activity_stats decoder] decodes a [activity_stats] value from [decoder] *)

val decode_repo_summary : Js.Json.t Js.Dict.t -> SearchTypes.repo_summary
(** [decode_repo_summary decoder] decodes a [repo_summary] value from [decoder] *)

val decode_repos_summary : Js.Json.t Js.Dict.t -> SearchTypes.repos_summary
(** [decode_repos_summary decoder] decodes a [repos_summary] value from [decoder] *)

val decode_term_count : Js.Json.t Js.Dict.t -> SearchTypes.term_count
(** [decode_term_count decoder] decodes a [term_count] value from [decoder] *)

val decode_terms_count : Js.Json.t Js.Dict.t -> SearchTypes.terms_count
(** [decode_terms_count decoder] decodes a [terms_count] value from [decoder] *)

val decode_author_peer : Js.Json.t Js.Dict.t -> SearchTypes.author_peer
(** [decode_author_peer decoder] decodes a [author_peer] value from [decoder] *)

val decode_authors_peers : Js.Json.t Js.Dict.t -> SearchTypes.authors_peers
(** [decode_authors_peers decoder] decodes a [authors_peers] value from [decoder] *)

val decode_lifecycle_stats : Js.Json.t Js.Dict.t -> SearchTypes.lifecycle_stats
(** [decode_lifecycle_stats decoder] decodes a [lifecycle_stats] value from [decoder] *)

val decode_changes_tops : Js.Json.t Js.Dict.t -> SearchTypes.changes_tops
(** [decode_changes_tops decoder] decodes a [changes_tops] value from [decoder] *)

val decode_query_response : Js.Json.t Js.Dict.t -> SearchTypes.query_response
(** [decode_query_response decoder] decodes a [query_response] value from [decoder] *)
