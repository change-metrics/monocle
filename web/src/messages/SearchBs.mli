(** search.proto BuckleScript Encoding *)


(** {2 Protobuf JSON Encoding} *)

val encode_search_suggestions_request : SearchTypes.search_suggestions_request -> Js.Json.t Js.Dict.t
(** [encode_search_suggestions_request v dict] encodes [v] int the given JSON [dict] *)

val encode_search_suggestions_response : SearchTypes.search_suggestions_response -> Js.Json.t Js.Dict.t
(** [encode_search_suggestions_response v dict] encodes [v] int the given JSON [dict] *)

val encode_query_error : SearchTypes.query_error -> Js.Json.t Js.Dict.t
(** [encode_query_error v dict] encodes [v] int the given JSON [dict] *)

val encode_changes_query_request : SearchTypes.changes_query_request -> Js.Json.t Js.Dict.t
(** [encode_changes_query_request v dict] encodes [v] int the given JSON [dict] *)

val encode_change : SearchTypes.change -> Js.Json.t Js.Dict.t
(** [encode_change v dict] encodes [v] int the given JSON [dict] *)

val encode_changes : SearchTypes.changes -> Js.Json.t Js.Dict.t
(** [encode_changes v dict] encodes [v] int the given JSON [dict] *)

val encode_changes_query_response : SearchTypes.changes_query_response -> Js.Json.t Js.Dict.t
(** [encode_changes_query_response v dict] encodes [v] int the given JSON [dict] *)


(** {2 BS Decoding} *)

val decode_search_suggestions_request : Js.Json.t Js.Dict.t -> SearchTypes.search_suggestions_request
(** [decode_search_suggestions_request decoder] decodes a [search_suggestions_request] value from [decoder] *)

val decode_search_suggestions_response : Js.Json.t Js.Dict.t -> SearchTypes.search_suggestions_response
(** [decode_search_suggestions_response decoder] decodes a [search_suggestions_response] value from [decoder] *)

val decode_query_error : Js.Json.t Js.Dict.t -> SearchTypes.query_error
(** [decode_query_error decoder] decodes a [query_error] value from [decoder] *)

val decode_changes_query_request : Js.Json.t Js.Dict.t -> SearchTypes.changes_query_request
(** [decode_changes_query_request decoder] decodes a [changes_query_request] value from [decoder] *)

val decode_change : Js.Json.t Js.Dict.t -> SearchTypes.change
(** [decode_change decoder] decodes a [change] value from [decoder] *)

val decode_changes : Js.Json.t Js.Dict.t -> SearchTypes.changes
(** [decode_changes decoder] decodes a [changes] value from [decoder] *)

val decode_changes_query_response : Js.Json.t Js.Dict.t -> SearchTypes.changes_query_response
(** [decode_changes_query_response decoder] decodes a [changes_query_response] value from [decoder] *)
