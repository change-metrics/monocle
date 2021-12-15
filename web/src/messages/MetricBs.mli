(** metric.proto BuckleScript Encoding *)


(** {2 Protobuf JSON Encoding} *)

val encode_metric_info : MetricTypes.metric_info -> Js.Json.t Js.Dict.t
(** [encode_metric_info v dict] encodes [v] int the given JSON [dict] *)

val encode_list_request : MetricTypes.list_request -> Js.Json.t Js.Dict.t
(** [encode_list_request v dict] encodes [v] int the given JSON [dict] *)

val encode_list_response : MetricTypes.list_response -> Js.Json.t Js.Dict.t
(** [encode_list_response v dict] encodes [v] int the given JSON [dict] *)


(** {2 BS Decoding} *)

val decode_metric_info : Js.Json.t Js.Dict.t -> MetricTypes.metric_info
(** [decode_metric_info decoder] decodes a [metric_info] value from [decoder] *)

val decode_list_request : Js.Json.t Js.Dict.t -> MetricTypes.list_request
(** [decode_list_request decoder] decodes a [list_request] value from [decoder] *)

val decode_list_response : Js.Json.t Js.Dict.t -> MetricTypes.list_response
(** [decode_list_response decoder] decodes a [list_response] value from [decoder] *)
