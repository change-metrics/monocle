(** login.proto Types *)



(** {2 Types} *)

type login_validation_request = {
  username : string;
}

type login_validation_response_validation_result =
  | Unknown_ident 
  | Known_ident 

type login_validation_response =
  | Validation_result of login_validation_response_validation_result


(** {2 Default values} *)

val default_login_validation_request : 
  ?username:string ->
  unit ->
  login_validation_request
(** [default_login_validation_request ()] is the default value for type [login_validation_request] *)

val default_login_validation_response_validation_result : unit -> login_validation_response_validation_result
(** [default_login_validation_response_validation_result ()] is the default value for type [login_validation_response_validation_result] *)

val default_login_validation_response : unit -> login_validation_response
(** [default_login_validation_response ()] is the default value for type [login_validation_response] *)
