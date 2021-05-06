(** timestamp.proto BuckleScript Encoding *)


(** {2 Protobuf JSON Encoding} *)

val encode_timestamp : TimestampTypes.timestamp -> Js.Json.t Js.Dict.t
(** [encode_timestamp v dict] encodes [v] int the given JSON [dict] *)


(** {2 BS Decoding} *)

val decode_timestamp : Js.Json.t Js.Dict.t -> TimestampTypes.timestamp
(** [decode_timestamp decoder] decodes a [timestamp] value from [decoder] *)
