[@@@ocaml.warning "-27-30-39"]


type login_validation_request = {
  username : string;
}

type login_validation_response_validation_result =
  | Unknown_ident 
  | Known_ident 

type login_validation_response =
  | Validation_result of login_validation_response_validation_result

let rec default_login_validation_request 
  ?username:((username:string) = "")
  () : login_validation_request  = {
  username;
}

let rec default_login_validation_response_validation_result () = (Unknown_ident:login_validation_response_validation_result)

let rec default_login_validation_response () : login_validation_response = Validation_result (default_login_validation_response_validation_result ())
