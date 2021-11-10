module RemoteData = {
  type t<'data> = option<result<'data, string>>

  let fmap = (m: t<'a>, f: 'a => 'b): t<'b> => {
    m->Belt.Option.flatMap(r => r->Belt.Result.flatMap(d => d->f->Ok)->Some)
  }
}

module UrlData = {
  let getParam = name => {
    let params = Prelude.URLSearchParams.current()
    params->Prelude.URLSearchParams.get(name)->Js.Nullable.toOption->Belt.Option.getWithDefault("")
  }
  let getOrder = () =>
    getParam("o")
    ->Prelude.orderFromQS
    ->Belt.Option.getWithDefault({field: "updated_at", direction: Desc})
    ->Some
  let getQuery = () =>
    switch getParam("q") {
    | "" => "from:now-3weeks"
    | q => q
    }
  let getFilter = () => getParam("f")
  let getLimit = () => {
    let params = Prelude.URLSearchParams.current()
    params
    ->Prelude.URLSearchParams.get("l")
    ->Js.Nullable.toOption
    ->Belt.Option.getWithDefault("0")
    ->int_of_string
  }
}

module Store = {
  type suggestionsR = RemoteData.t<SearchTypes.suggestions_response>
  type fieldsRespR = RemoteData.t<SearchTypes.fields_response>
  type userGroupsR = RemoteData.t<UserGroupTypes.list_response>
  type projectsR = RemoteData.t<ConfigTypes.get_projects_response>
  type aboutR = RemoteData.t<ConfigTypes.get_about_response>

  type t = {
    index: string,
    query: string,
    filter: string,
    limit: int,
    order: option<SearchTypes.order>,
    suggestions: suggestionsR,
    fields: RemoteData.t<list<SearchTypes.field>>,
    user_groups: userGroupsR,
    projects: projectsR,
    changes_pies_panel: bool,
    about: aboutR,
    dexie: Dexie.Database.t,
    toasts: list<string>,
  }
  type action =
    | ChangeIndex(string)
    | SetQuery(string)
    | SetFilter(string)
    | SetLimit(int)
    | SetOrder(option<SearchTypes.order>)
    | FetchFields(fieldsRespR)
    | FetchSuggestions(suggestionsR)
    | FetchUserGroups(userGroupsR)
    | FetchProjects(projectsR)
    | FetchAbout(aboutR)
    | ReverseChangesPiePanelState
    | RemoveToast(string)
    | AddToast(string)
  type dispatch = action => unit

  let create = index => {
    index: index,
    query: UrlData.getQuery(),
    filter: UrlData.getFilter(),
    limit: UrlData.getLimit(),
    order: UrlData.getOrder(),
    suggestions: None,
    fields: None,
    user_groups: None,
    projects: None,
    about: None,
    changes_pies_panel: false,
    dexie: MonoIndexedDB.mkDexie(),
    toasts: list{},
  }

  let reducer = (state: t, action: action) =>
    switch action {
    | RemoveToast(toast) => {...state, toasts: state.toasts->Belt.List.keep(x => x != toast)}
    | AddToast(toast) => {
        ...state,
        toasts: state.toasts->Belt.List.keep(x => x != toast)->Belt.List.add(toast),
      }
    | ChangeIndex(index) => {
        RescriptReactRouter.push("/" ++ index)
        create(index)
      }
    | SetQuery(query) => {
        Prelude.setLocationSearch("q", query)->ignore
        {...state, query: query}
      }
    | SetFilter(query) => {
        Prelude.setLocationSearch("f", query)->ignore
        {...state, filter: query}
      }
    | SetOrder(order) => {
        Prelude.setLocationSearch("o", order->Prelude.orderToQS)->ignore
        {...state, order: order}
      }
    | SetLimit(limit) => {
        Prelude.setLocationSearch("l", limit->string_of_int)->ignore
        {...state, limit: limit}
      }
    | FetchFields(res) => {...state, fields: res->RemoteData.fmap(resp => resp.fields)}
    | FetchSuggestions(res) => {...state, suggestions: res}
    | FetchUserGroups(res) => {...state, user_groups: res}
    | FetchProjects(res) => {...state, projects: res}
    | FetchAbout(res) => {...state, about: res}
    | ReverseChangesPiePanelState => {...state, changes_pies_panel: !state.changes_pies_panel}
    }
}

module Fetch = {
  // Helper module to abstract the WebApi
  open WebApi
  let fetch = (
    value: RemoteData.t<'r>,
    get: unit => axios<'a>,
    mkAction: RemoteData.t<'a> => Store.action,
    dispatch: Store.dispatch,
  ) => {
    let set = v => v->Some->mkAction->dispatch->Js.Promise.resolve
    let handleErr = err => {
      Js.log(err)
      "Network error"->Error->set
    }
    let handleOk = resp => resp.data->Ok->set
    // Effect0 is performed when the component is monted
    React.useEffect0(() => {
      // We fetch the remote data only when needed
      switch value {
      | None => (get() |> Js.Promise.then_(handleOk) |> Js.Promise.catch(handleErr))->ignore
      | _ => ignore()
      }
      None
    })
    value
  }

  let suggestions = ((state: Store.t, dispatch)) =>
    fetch(
      state.suggestions,
      () => WebApi.Search.suggestions({SearchTypes.index: state.index}),
      res => Store.FetchSuggestions(res),
      dispatch,
    )

  let fields = ((state: Store.t, dispatch)) => {
    fetch(
      state.fields,
      () => WebApi.Search.fields({version: "1"}),
      res => Store.FetchFields(res),
      dispatch,
    )
  }

  let user_groups = ((state: Store.t, dispatch)) => {
    fetch(
      state.user_groups,
      () => WebApi.UserGroup.list({UserGroupTypes.index: state.index}),
      res => Store.FetchUserGroups(res),
      dispatch,
    )
  }

  let projects = ((state: Store.t, dispatch)) => {
    fetch(
      state.projects,
      () => WebApi.Config.getProjects({ConfigTypes.index: state.index}),
      res => Store.FetchProjects(res),
      dispatch,
    )
  }

  let about = ((state: Store.t, dispatch)) => {
    fetch(
      state.about,
      () => WebApi.Config.getAbout({void: ""}),
      res => Store.FetchAbout(res),
      dispatch,
    )
  }
}

let changeIndex = ((_, dispatch), name) => name->Store.ChangeIndex->dispatch

let mkSearchRequest = (state: Store.t, query_type: SearchTypes.query_request_query_type) => {
  SearchTypes.index: state.index,
  username: "",
  query: state.query,
  query_type: query_type,
  order: state.order,
  limit: state.limit->Int32.of_int,
  change_id: "",
}

// Hook API
type t = (Store.t, Store.action => unit)

let use = (index): t => React.useReducer(Store.reducer, Store.create(index))
