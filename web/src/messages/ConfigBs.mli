(** config.proto BuckleScript Encoding *)


(** {2 Protobuf JSON Encoding} *)

val encode_project_definition : ConfigTypes.project_definition -> Js.Json.t Js.Dict.t
(** [encode_project_definition v dict] encodes [v] int the given JSON [dict] *)

val encode_get_projects_request : ConfigTypes.get_projects_request -> Js.Json.t Js.Dict.t
(** [encode_get_projects_request v dict] encodes [v] int the given JSON [dict] *)

val encode_get_projects_response : ConfigTypes.get_projects_response -> Js.Json.t Js.Dict.t
(** [encode_get_projects_response v dict] encodes [v] int the given JSON [dict] *)

val encode_workspace : ConfigTypes.workspace -> Js.Json.t Js.Dict.t
(** [encode_workspace v dict] encodes [v] int the given JSON [dict] *)

val encode_get_workspaces_request : ConfigTypes.get_workspaces_request -> Js.Json.t Js.Dict.t
(** [encode_get_workspaces_request v dict] encodes [v] int the given JSON [dict] *)

val encode_get_workspaces_response : ConfigTypes.get_workspaces_response -> Js.Json.t Js.Dict.t
(** [encode_get_workspaces_response v dict] encodes [v] int the given JSON [dict] *)

val encode_about_about_link : ConfigTypes.about_about_link -> Js.Json.t Js.Dict.t
(** [encode_about_about_link v dict] encodes [v] int the given JSON [dict] *)

val encode_about : ConfigTypes.about -> Js.Json.t Js.Dict.t
(** [encode_about v dict] encodes [v] int the given JSON [dict] *)

val encode_get_about_request : ConfigTypes.get_about_request -> Js.Json.t Js.Dict.t
(** [encode_get_about_request v dict] encodes [v] int the given JSON [dict] *)

val encode_get_about_response : ConfigTypes.get_about_response -> Js.Json.t Js.Dict.t
(** [encode_get_about_response v dict] encodes [v] int the given JSON [dict] *)


(** {2 BS Decoding} *)

val decode_project_definition : Js.Json.t Js.Dict.t -> ConfigTypes.project_definition
(** [decode_project_definition decoder] decodes a [project_definition] value from [decoder] *)

val decode_get_projects_request : Js.Json.t Js.Dict.t -> ConfigTypes.get_projects_request
(** [decode_get_projects_request decoder] decodes a [get_projects_request] value from [decoder] *)

val decode_get_projects_response : Js.Json.t Js.Dict.t -> ConfigTypes.get_projects_response
(** [decode_get_projects_response decoder] decodes a [get_projects_response] value from [decoder] *)

val decode_workspace : Js.Json.t Js.Dict.t -> ConfigTypes.workspace
(** [decode_workspace decoder] decodes a [workspace] value from [decoder] *)

val decode_get_workspaces_request : Js.Json.t Js.Dict.t -> ConfigTypes.get_workspaces_request
(** [decode_get_workspaces_request decoder] decodes a [get_workspaces_request] value from [decoder] *)

val decode_get_workspaces_response : Js.Json.t Js.Dict.t -> ConfigTypes.get_workspaces_response
(** [decode_get_workspaces_response decoder] decodes a [get_workspaces_response] value from [decoder] *)

val decode_about_about_link : Js.Json.t Js.Dict.t -> ConfigTypes.about_about_link
(** [decode_about_about_link decoder] decodes a [about_about_link] value from [decoder] *)

val decode_about : Js.Json.t Js.Dict.t -> ConfigTypes.about
(** [decode_about decoder] decodes a [about] value from [decoder] *)

val decode_get_about_request : Js.Json.t Js.Dict.t -> ConfigTypes.get_about_request
(** [decode_get_about_request decoder] decodes a [get_about_request] value from [decoder] *)

val decode_get_about_response : Js.Json.t Js.Dict.t -> ConfigTypes.get_about_response
(** [decode_get_about_response decoder] decodes a [get_about_response] value from [decoder] *)
