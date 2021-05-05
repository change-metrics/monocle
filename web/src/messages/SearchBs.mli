(** search.proto BuckleScript Encoding *)


(** {2 Protobuf JSON Encoding} *)

val encode_search_suggestions_request : SearchTypes.search_suggestions_request -> Js.Json.t Js.Dict.t
(** [encode_search_suggestions_request v dict] encodes [v] int the given JSON [dict] *)

val encode_search_suggestions_response : SearchTypes.search_suggestions_response -> Js.Json.t Js.Dict.t
(** [encode_search_suggestions_response v dict] encodes [v] int the given JSON [dict] *)


(** {2 BS Decoding} *)

val decode_search_suggestions_request : Js.Json.t Js.Dict.t -> SearchTypes.search_suggestions_request
(** [decode_search_suggestions_request decoder] decodes a [search_suggestions_request] value from [decoder] *)

val decode_search_suggestions_response : Js.Json.t Js.Dict.t -> SearchTypes.search_suggestions_response
(** [decode_search_suggestions_response decoder] decodes a [search_suggestions_response] value from [decoder] *)
