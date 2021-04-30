(** config.proto BuckleScript Encoding *)


(** {2 Protobuf JSON Encoding} *)

val encode_project_definition : ConfigTypes.project_definition -> Js.Json.t Js.Dict.t
(** [encode_project_definition v dict] encodes [v] int the given JSON [dict] *)

val encode_get_projects_request : ConfigTypes.get_projects_request -> Js.Json.t Js.Dict.t
(** [encode_get_projects_request v dict] encodes [v] int the given JSON [dict] *)

val encode_get_projects_response : ConfigTypes.get_projects_response -> Js.Json.t Js.Dict.t
(** [encode_get_projects_response v dict] encodes [v] int the given JSON [dict] *)


(** {2 BS Decoding} *)

val decode_project_definition : Js.Json.t Js.Dict.t -> ConfigTypes.project_definition
(** [decode_project_definition decoder] decodes a [project_definition] value from [decoder] *)

val decode_get_projects_request : Js.Json.t Js.Dict.t -> ConfigTypes.get_projects_request
(** [decode_get_projects_request decoder] decodes a [get_projects_request] value from [decoder] *)

val decode_get_projects_response : Js.Json.t Js.Dict.t -> ConfigTypes.get_projects_response
(** [decode_get_projects_response decoder] decodes a [get_projects_response] value from [decoder] *)
