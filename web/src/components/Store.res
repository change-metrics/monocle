module RemoteData = {
  type t<'data> = option<result<'data, string>>

  let fmap = (m: t<'a>, f: 'a => 'b): t<'b> => {
    m->Belt.Option.flatMap(r => r->Belt.Result.flatMap(d => d->f->Ok)->Some)
  }
}

module UrlData = {
  let getQuery = () => {
    let params = Prelude.URLSearchParams.current()
    switch params->Prelude.URLSearchParams.get("q")->Js.Nullable.toOption {
    | Some(expr) => expr
    | None => "limit 5"
    }
  }
}

module Session = {
  type t = Unknown | Anonymous | Authenticated(string)
}

module Store = {
  type suggestionsR = RemoteData.t<SearchTypes.search_suggestions_response>
  type fieldsRespR = RemoteData.t<SearchTypes.fields_response>

  type t = {
    index: string,
    query: string,
    session: Session.t,
    suggestions: suggestionsR,
    fields: RemoteData.t<list<SearchTypes.field>>,
  }
  type action =
    | ChangeIndex(string)
    | SetQuery(string)
    | SetSession(Session.t)
    | FetchFields(fieldsRespR)
    | FetchSuggestions(suggestionsR)
  type dispatch = action => unit

  let reducer = (state: t, action: action) =>
    switch action {
    | ChangeIndex(index) => {...state, index: index}
    | SetQuery(query) => {
        Prelude.setLocationSearch("q", query)->ignore
        {...state, query: query}
      }
    | SetSession(session) => {...state, session: session}
    | FetchFields(res) => {...state, fields: res->RemoteData.fmap(resp => resp.fields)}
    | FetchSuggestions(res) => {...state, suggestions: res}
    }

  // TODO: replace static index with a SetIndex action, after the LegacyApp is removed
  let create = index => {
    index: index,
    query: UrlData.getQuery(),
    session: Session.Unknown,
    suggestions: None,
    fields: None,
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
}

let changeIndex = ((_, dispatch), name) => name->Store.ChangeIndex->dispatch

// Hook API
type t = (Store.t, Store.action => unit)

let use = (index): t => React.useReducer(Store.reducer, Store.create(index))
