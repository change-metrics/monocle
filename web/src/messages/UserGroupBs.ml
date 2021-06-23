[@@@ocaml.warning "-27-30-39"]

type group_definition_mutable = {
  mutable name : string;
  mutable members : int32;
}

let default_group_definition_mutable () : group_definition_mutable = {
  name = "";
  members = 0l;
}

type list_request_mutable = {
  mutable index : string;
}

let default_list_request_mutable () : list_request_mutable = {
  index = "";
}

type list_response_mutable = {
  mutable items : UserGroupTypes.group_definition list;
}

let default_list_response_mutable () : list_response_mutable = {
  items = [];
}


let rec decode_group_definition json =
  let v = default_group_definition_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "name" -> 
      let json = Js.Dict.unsafeGet json "name" in
      v.name <- Pbrt_bs.string json "group_definition" "name"
    | "members" -> 
      let json = Js.Dict.unsafeGet json "members" in
      v.members <- Pbrt_bs.int32 json "group_definition" "members"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    UserGroupTypes.name = v.name;
    UserGroupTypes.members = v.members;
  } : UserGroupTypes.group_definition)

let rec decode_list_request json =
  let v = default_list_request_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "index" -> 
      let json = Js.Dict.unsafeGet json "index" in
      v.index <- Pbrt_bs.string json "list_request" "index"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    UserGroupTypes.index = v.index;
  } : UserGroupTypes.list_request)

let rec decode_list_response json =
  let v = default_list_response_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "items" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "items" in 
        Pbrt_bs.array_ a "list_response" "items"
      in
      v.items <- Array.map (fun json -> 
        (decode_group_definition (Pbrt_bs.object_ json "list_response" "items"))
      ) a |> Array.to_list;
    end
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    UserGroupTypes.items = v.items;
  } : UserGroupTypes.list_response)

let rec encode_group_definition (v:UserGroupTypes.group_definition) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "name" (Js.Json.string v.UserGroupTypes.name);
  Js.Dict.set json "members" (Js.Json.number (Int32.to_float v.UserGroupTypes.members));
  json

let rec encode_list_request (v:UserGroupTypes.list_request) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "index" (Js.Json.string v.UserGroupTypes.index);
  json

let rec encode_list_response (v:UserGroupTypes.list_response) = 
  let json = Js.Dict.empty () in
  begin (* items field *)
    let (items':Js.Json.t) =
      v.UserGroupTypes.items
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_group_definition |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "items" items';
  end;
  json
