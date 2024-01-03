(** crawler.proto BuckleScript Encoding *)


(** {2 Protobuf JSON Encoding} *)

val encode_entity : CrawlerTypes.entity -> Js.Json.t Js.Dict.t
(** [encode_entity v dict] encodes [v] int the given JSON [dict] *)

val encode_entity_type : CrawlerTypes.entity_type -> string
(** [encode_entity_type v] returns JSON string*)

val encode_crawler_error : CrawlerTypes.crawler_error -> Js.Json.t Js.Dict.t
(** [encode_crawler_error v dict] encodes [v] int the given JSON [dict] *)

val encode_crawler_error_list : CrawlerTypes.crawler_error_list -> Js.Json.t Js.Dict.t
(** [encode_crawler_error_list v dict] encodes [v] int the given JSON [dict] *)

val encode_errors_request : CrawlerTypes.errors_request -> Js.Json.t Js.Dict.t
(** [encode_errors_request v dict] encodes [v] int the given JSON [dict] *)

val encode_errors_list : CrawlerTypes.errors_list -> Js.Json.t Js.Dict.t
(** [encode_errors_list v dict] encodes [v] int the given JSON [dict] *)

val encode_errors_response : CrawlerTypes.errors_response -> Js.Json.t Js.Dict.t
(** [encode_errors_response v dict] encodes [v] int the given JSON [dict] *)

val encode_project : CrawlerTypes.project -> Js.Json.t Js.Dict.t
(** [encode_project v dict] encodes [v] int the given JSON [dict] *)

val encode_add_doc_request : CrawlerTypes.add_doc_request -> Js.Json.t Js.Dict.t
(** [encode_add_doc_request v dict] encodes [v] int the given JSON [dict] *)

val encode_add_doc_error : CrawlerTypes.add_doc_error -> string
(** [encode_add_doc_error v] returns JSON string*)

val encode_add_doc_response : CrawlerTypes.add_doc_response -> Js.Json.t Js.Dict.t
(** [encode_add_doc_response v dict] encodes [v] int the given JSON [dict] *)

val encode_commit_request : CrawlerTypes.commit_request -> Js.Json.t Js.Dict.t
(** [encode_commit_request v dict] encodes [v] int the given JSON [dict] *)

val encode_commit_error : CrawlerTypes.commit_error -> string
(** [encode_commit_error v] returns JSON string*)

val encode_commit_response : CrawlerTypes.commit_response -> Js.Json.t Js.Dict.t
(** [encode_commit_response v dict] encodes [v] int the given JSON [dict] *)

val encode_commit_info_request : CrawlerTypes.commit_info_request -> Js.Json.t Js.Dict.t
(** [encode_commit_info_request v dict] encodes [v] int the given JSON [dict] *)

val encode_commit_info_error : CrawlerTypes.commit_info_error -> string
(** [encode_commit_info_error v] returns JSON string*)

val encode_commit_info_response_oldest_entity : CrawlerTypes.commit_info_response_oldest_entity -> Js.Json.t Js.Dict.t
(** [encode_commit_info_response_oldest_entity v dict] encodes [v] int the given JSON [dict] *)

val encode_commit_info_response : CrawlerTypes.commit_info_response -> Js.Json.t Js.Dict.t
(** [encode_commit_info_response v dict] encodes [v] int the given JSON [dict] *)


(** {2 BS Decoding} *)

val decode_entity : Js.Json.t Js.Dict.t -> CrawlerTypes.entity
(** [decode_entity decoder] decodes a [entity] value from [decoder] *)

val decode_entity_type : Js.Json.t -> CrawlerTypes.entity_type
(** [decode_entity_type value] decodes a [entity_type] from a Json value*)

val decode_crawler_error : Js.Json.t Js.Dict.t -> CrawlerTypes.crawler_error
(** [decode_crawler_error decoder] decodes a [crawler_error] value from [decoder] *)

val decode_crawler_error_list : Js.Json.t Js.Dict.t -> CrawlerTypes.crawler_error_list
(** [decode_crawler_error_list decoder] decodes a [crawler_error_list] value from [decoder] *)

val decode_errors_request : Js.Json.t Js.Dict.t -> CrawlerTypes.errors_request
(** [decode_errors_request decoder] decodes a [errors_request] value from [decoder] *)

val decode_errors_list : Js.Json.t Js.Dict.t -> CrawlerTypes.errors_list
(** [decode_errors_list decoder] decodes a [errors_list] value from [decoder] *)

val decode_errors_response : Js.Json.t Js.Dict.t -> CrawlerTypes.errors_response
(** [decode_errors_response decoder] decodes a [errors_response] value from [decoder] *)

val decode_project : Js.Json.t Js.Dict.t -> CrawlerTypes.project
(** [decode_project decoder] decodes a [project] value from [decoder] *)

val decode_add_doc_request : Js.Json.t Js.Dict.t -> CrawlerTypes.add_doc_request
(** [decode_add_doc_request decoder] decodes a [add_doc_request] value from [decoder] *)

val decode_add_doc_error : Js.Json.t -> CrawlerTypes.add_doc_error
(** [decode_add_doc_error value] decodes a [add_doc_error] from a Json value*)

val decode_add_doc_response : Js.Json.t Js.Dict.t -> CrawlerTypes.add_doc_response
(** [decode_add_doc_response decoder] decodes a [add_doc_response] value from [decoder] *)

val decode_commit_request : Js.Json.t Js.Dict.t -> CrawlerTypes.commit_request
(** [decode_commit_request decoder] decodes a [commit_request] value from [decoder] *)

val decode_commit_error : Js.Json.t -> CrawlerTypes.commit_error
(** [decode_commit_error value] decodes a [commit_error] from a Json value*)

val decode_commit_response : Js.Json.t Js.Dict.t -> CrawlerTypes.commit_response
(** [decode_commit_response decoder] decodes a [commit_response] value from [decoder] *)

val decode_commit_info_request : Js.Json.t Js.Dict.t -> CrawlerTypes.commit_info_request
(** [decode_commit_info_request decoder] decodes a [commit_info_request] value from [decoder] *)

val decode_commit_info_error : Js.Json.t -> CrawlerTypes.commit_info_error
(** [decode_commit_info_error value] decodes a [commit_info_error] from a Json value*)

val decode_commit_info_response_oldest_entity : Js.Json.t Js.Dict.t -> CrawlerTypes.commit_info_response_oldest_entity
(** [decode_commit_info_response_oldest_entity decoder] decodes a [commit_info_response_oldest_entity] value from [decoder] *)

val decode_commit_info_response : Js.Json.t Js.Dict.t -> CrawlerTypes.commit_info_response
(** [decode_commit_info_response decoder] decodes a [commit_info_response] value from [decoder] *)
