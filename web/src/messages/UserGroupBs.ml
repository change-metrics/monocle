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

type review_histo_mutable = {
  mutable date : int64;
  mutable count : int32;
}

let default_review_histo_mutable () : review_histo_mutable = {
  date = 0L;
  count = 0l;
}

type group_stat_mutable = {
  mutable change_review_ratio : float;
  mutable author_review_ratio : float;
  mutable commit_histo : UserGroupTypes.review_histo list;
  mutable review_histo : UserGroupTypes.review_histo list;
}

let default_group_stat_mutable () : group_stat_mutable = {
  change_review_ratio = 0.;
  author_review_ratio = 0.;
  commit_histo = [];
  review_histo = [];
}

type user_stat_mutable = {
  mutable name : string;
  mutable stat : UserGroupTypes.group_stat option;
}

let default_user_stat_mutable () : user_stat_mutable = {
  name = "";
  stat = None;
}

type get_request_mutable = {
  mutable index : string;
  mutable name : string;
  mutable query : string;
}

let default_get_request_mutable () : get_request_mutable = {
  index = "";
  name = "";
  query = "";
}

type get_response_mutable = {
  mutable all : UserGroupTypes.group_stat option;
  mutable users : UserGroupTypes.user_stat list;
}

let default_get_response_mutable () : get_response_mutable = {
  all = None;
  users = [];
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

let rec decode_review_histo json =
  let v = default_review_histo_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "date" -> 
      let json = Js.Dict.unsafeGet json "date" in
      v.date <- Pbrt_bs.int64 json "review_histo" "date"
    | "count" -> 
      let json = Js.Dict.unsafeGet json "count" in
      v.count <- Pbrt_bs.int32 json "review_histo" "count"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    UserGroupTypes.date = v.date;
    UserGroupTypes.count = v.count;
  } : UserGroupTypes.review_histo)

let rec decode_group_stat json =
  let v = default_group_stat_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "change_review_ratio" -> 
      let json = Js.Dict.unsafeGet json "change_review_ratio" in
      v.change_review_ratio <- Pbrt_bs.float json "group_stat" "change_review_ratio"
    | "author_review_ratio" -> 
      let json = Js.Dict.unsafeGet json "author_review_ratio" in
      v.author_review_ratio <- Pbrt_bs.float json "group_stat" "author_review_ratio"
    | "commit_histo" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "commit_histo" in 
        Pbrt_bs.array_ a "group_stat" "commit_histo"
      in
      v.commit_histo <- Array.map (fun json -> 
        (decode_review_histo (Pbrt_bs.object_ json "group_stat" "commit_histo"))
      ) a |> Array.to_list;
    end
    | "review_histo" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "review_histo" in 
        Pbrt_bs.array_ a "group_stat" "review_histo"
      in
      v.review_histo <- Array.map (fun json -> 
        (decode_review_histo (Pbrt_bs.object_ json "group_stat" "review_histo"))
      ) a |> Array.to_list;
    end
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    UserGroupTypes.change_review_ratio = v.change_review_ratio;
    UserGroupTypes.author_review_ratio = v.author_review_ratio;
    UserGroupTypes.commit_histo = v.commit_histo;
    UserGroupTypes.review_histo = v.review_histo;
  } : UserGroupTypes.group_stat)

let rec decode_user_stat json =
  let v = default_user_stat_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "name" -> 
      let json = Js.Dict.unsafeGet json "name" in
      v.name <- Pbrt_bs.string json "user_stat" "name"
    | "stat" -> 
      let json = Js.Dict.unsafeGet json "stat" in
      v.stat <- Some ((decode_group_stat (Pbrt_bs.object_ json "user_stat" "stat")))
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    UserGroupTypes.name = v.name;
    UserGroupTypes.stat = v.stat;
  } : UserGroupTypes.user_stat)

let rec decode_get_request json =
  let v = default_get_request_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "index" -> 
      let json = Js.Dict.unsafeGet json "index" in
      v.index <- Pbrt_bs.string json "get_request" "index"
    | "name" -> 
      let json = Js.Dict.unsafeGet json "name" in
      v.name <- Pbrt_bs.string json "get_request" "name"
    | "query" -> 
      let json = Js.Dict.unsafeGet json "query" in
      v.query <- Pbrt_bs.string json "get_request" "query"
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    UserGroupTypes.index = v.index;
    UserGroupTypes.name = v.name;
    UserGroupTypes.query = v.query;
  } : UserGroupTypes.get_request)

let rec decode_get_response json =
  let v = default_get_response_mutable () in
  let keys = Js.Dict.keys json in
  let last_key_index = Array.length keys - 1 in
  for i = 0 to last_key_index do
    match Array.unsafe_get keys i with
    | "all" -> 
      let json = Js.Dict.unsafeGet json "all" in
      v.all <- Some ((decode_group_stat (Pbrt_bs.object_ json "get_response" "all")))
    | "users" -> begin
      let a = 
        let a = Js.Dict.unsafeGet json "users" in 
        Pbrt_bs.array_ a "get_response" "users"
      in
      v.users <- Array.map (fun json -> 
        (decode_user_stat (Pbrt_bs.object_ json "get_response" "users"))
      ) a |> Array.to_list;
    end
    
    | _ -> () (*Unknown fields are ignored*)
  done;
  ({
    UserGroupTypes.all = v.all;
    UserGroupTypes.users = v.users;
  } : UserGroupTypes.get_response)

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

let rec encode_review_histo (v:UserGroupTypes.review_histo) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "date" (Js.Json.string (Int64.to_string v.UserGroupTypes.date));
  Js.Dict.set json "count" (Js.Json.number (Int32.to_float v.UserGroupTypes.count));
  json

let rec encode_group_stat (v:UserGroupTypes.group_stat) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "change_review_ratio" (Js.Json.number v.UserGroupTypes.change_review_ratio);
  Js.Dict.set json "author_review_ratio" (Js.Json.number v.UserGroupTypes.author_review_ratio);
  begin (* commitHisto field *)
    let (commit_histo':Js.Json.t) =
      v.UserGroupTypes.commit_histo
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_review_histo |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "commit_histo" commit_histo';
  end;
  begin (* reviewHisto field *)
    let (review_histo':Js.Json.t) =
      v.UserGroupTypes.review_histo
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_review_histo |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "review_histo" review_histo';
  end;
  json

let rec encode_user_stat (v:UserGroupTypes.user_stat) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "name" (Js.Json.string v.UserGroupTypes.name);
  begin match v.UserGroupTypes.stat with
  | None -> ()
  | Some v ->
    begin (* stat field *)
      let json' = encode_group_stat v in
      Js.Dict.set json "stat" (Js.Json.object_ json');
    end;
  end;
  json

let rec encode_get_request (v:UserGroupTypes.get_request) = 
  let json = Js.Dict.empty () in
  Js.Dict.set json "index" (Js.Json.string v.UserGroupTypes.index);
  Js.Dict.set json "name" (Js.Json.string v.UserGroupTypes.name);
  Js.Dict.set json "query" (Js.Json.string v.UserGroupTypes.query);
  json

let rec encode_get_response (v:UserGroupTypes.get_response) = 
  let json = Js.Dict.empty () in
  begin match v.UserGroupTypes.all with
  | None -> ()
  | Some v ->
    begin (* all field *)
      let json' = encode_group_stat v in
      Js.Dict.set json "all" (Js.Json.object_ json');
    end;
  end;
  begin (* users field *)
    let (users':Js.Json.t) =
      v.UserGroupTypes.users
      |> Array.of_list
      |> Array.map (fun v ->
        v |> encode_user_stat |> Js.Json.object_
      )
      |> Js.Json.array
    in
    Js.Dict.set json "users" users';
  end;
  json
