[@@@ocaml.warning "-27-30-39"]

type search_suggestions_request_mutable = {
  mutable index : string;
}

let default_search_suggestions_request_mutable () : search_suggestions_request_mutable = {
  index = "";
}

type search_suggestions_response_mutable = {
  mutable task_types : string list;
  mutable authors : string list;
}

let default_search_suggestions_response_mutable () : search_suggestions_response_mutable = {
  task_types = [];
  authors = [];
}


let rec decode_search_suggestions_request json =
  let v = default_search_suggestions_request_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "index" -> 
      let json = Js.Dict.unsafeGet json "index" in
      v.index <- Pbrt_bs.string json "search_suggestions_request" "index"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    SearchTypes.index = v.index;
  } : SearchTypes.search_suggestions_request)

let rec decode_search_suggestions_response json =
  let v = default_search_suggestions_response_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "task_types" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "task_types" in 
        Pbrt_bs.array_ a "search_suggestions_response" "task_types"
      in
      v.task_types <- Array.map (fun json -> 
        Pbrt_bs.string json "search_suggestions_response" "task_types"
      ) a |> Array.to_list;
    end
    | "authors" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "authors" in 
        Pbrt_bs.array_ a "search_suggestions_response" "authors"
      in
      v.authors <- Array.map (fun json -> 
        Pbrt_bs.string json "search_suggestions_response" "authors"
      ) a |> Array.to_list;
    end
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    SearchTypes.task_types = v.task_types;
    SearchTypes.authors = v.authors;
  } : SearchTypes.search_suggestions_response)

let rec encode_search_suggestions_request (v:SearchTypes.search_suggestions_request) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "index" (Js.Json.string v.SearchTypes.index);
  json

let rec encode_search_suggestions_response (v:SearchTypes.search_suggestions_response) = 
  let json = Js.Dict.empty () in
  let a = v.SearchTypes.task_types |> Array.of_list |> Array.map Js.Json.string in
  Js.Dict.set json "task_types" (Js.Json.array a);
  let a = v.SearchTypes.authors |> Array.of_list |> Array.map Js.Json.string in
  Js.Dict.set json "authors" (Js.Json.array a);
  json
