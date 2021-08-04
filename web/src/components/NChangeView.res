open Prelude

module CPie = {
  type entry = {
    key: string,
    doc_count: int,
  }
  type t = {
    items: array<entry>,
    total_hits: int,
  }
  type palette_t = {
    "APPROVED": string,
    "CHANGES_REQUESTED": string,
    "COMMENTED": string,
    "Code-Review+1": string,
    "Code-Review+2": string,
    "Code-Review-1": string,
    "Code-Review-2": string,
    "DISMISSED": string,
    "Workflow+1": string,
    "Workflow-1": string,
  }
  @react.component @module("./pie_view.jsx")
  external make: (
    ~data: t,
    ~title: string,
    ~handleClick: (~value: string) => unit,
    ~palette: palette_t=?,
    ~other_label: string=?,
  ) => React.element = "default"
}

module ChangeList = {
  @react.component
  let make = (~store: Store.t, ~changes: array<Web.SearchTypes.change>) => {
    let (changesArray, paginate) = usePagination(changes)
    <>
      {paginate}
      <Patternfly.DataList isCompact={true}>
        {changesArray
        ->Belt.Array.map(change => <Change.DataItem store key={change.url} change={change} />)
        ->React.array}
      </Patternfly.DataList>
    </>
  }
}

module ChangesTopPies = {
  @react.component
  let make = (~store) => {
    let (state, dispatch) = store
    let qtype = SearchTypes.Query_changes_tops
    let request = {
      ...Store.mkSearchRequest(state, qtype),
      limit: 10->Int32.of_int,
      query: addQuery(state.query, state.filter),
    }
    let query = request.query
    let getEntry = (e: SearchTypes.term_count): CPie.entry => {
      doc_count: e.count->Int32.to_int,
      key: e.term,
    }
    let adapt = (elms: SearchTypes.terms_count, kf: CPie.entry => bool): CPie.t => {
      items: elms.termcount->Belt.List.map(getEntry)->Belt.List.keep(kf)->Belt.List.toArray,
      total_hits: elms.total_hits->Int32.to_int,
    }
    let tee = (_: bool => bool) => Store.Store.ReverseChangesPiePanelState->dispatch
    let approvals_palette = {
      "Code-Review+2": "#00ff9f",
      "Code-Review+1": "#B6FCD5",
      "Code-Review-1": "#CA5462",
      "Code-Review-2": "#AB0000",
      "Workflow+1": "#00ff9f",
      "Workflow-1": "#AB0000",
      "APPROVED": "#00ff9f",
      "DISMISSED": "#AB0000",
      "COMMENTED": "#B6FCD5",
      "CHANGES_REQUESTED": "#CA5462",
    }
    let ignoredApproval = ["Code-Review+0", "Verified+0", "Workflow+0", "COMMENTED"]
    let handlePieClick = (state: Store.Store.t, dispatch, ~field: string, ~value: string) => {
      let newFilter = field ++ ":\"" ++ value ++ "\""
      let filter = Js.String.includes(newFilter, state.filter)
        ? state.filter
        : addQuery(state.filter, newFilter)
      let base = "/" ++ state.index ++ "/" ++ "changes" ++ "?"
      let query = switch state.query {
      | "" => ""
      | query => "q=" ++ query ++ "&"
      }
      let href = base ++ query ++ "f=" ++ filter
      filter->Store.Store.SetFilter->dispatch
      href->RescriptReactRouter.push
    }
    switch useAutoGetOn(() => WebApi.Search.query(request), query) {
    | None => <Spinner />
    | Some(Error(title)) => <Alert variant=#Danger title />
    | Some(Ok(SearchTypes.Error(err))) =>
      <Alert
        title={err.message ++ " at " ++ string_of_int(Int32.to_int(err.position))} variant=#Danger
      />
    | Some(Ok(SearchTypes.Changes_tops(items))) =>
      <MExpandablePanel
        title={"Show changes pie charts"} stateControler={(state.changes_pies_panel, tee)}>
        <MGrid>
          <MGridItemXl4>
            <CPie
              data={items.authors->Belt.Option.getExn->adapt(_ => true)}
              title={"Changes per author"}
              handleClick={handlePieClick(state, dispatch, ~field="author")}
            />
          </MGridItemXl4>
          <MGridItemXl4>
            <CPie
              data={items.repos->Belt.Option.getExn->adapt(_ => true)}
              title={"Changes per repository"}
              handleClick={handlePieClick(state, dispatch, ~field="repo")}
            />
          </MGridItemXl4>
          <MGridItemXl4>
            <CPie
              palette=approvals_palette
              data={items.approvals
              ->Belt.Option.getExn
              ->adapt(e => ignoredApproval->Belt.Array.some(e' => e' != e.key))}
              title={"Changes per approval"}
              other_label={"No approval"}
              handleClick={handlePieClick(state, dispatch, ~field="approval")}
            />
          </MGridItemXl4>
        </MGrid>
      </MExpandablePanel>
    | Some(Ok(_)) => <Alert title={"Invalid response"} />
    }
  }
}

@react.component
let make = (~store: Store.t) => {
  let (state, _) = store
  let query = addQuery(state.query, state.filter)
  let request = {
    ...Store.mkSearchRequest(state, SearchTypes.Query_change),
    query: query,
  }

  <div>
    {switch useAutoGetOn(() => WebApi.Search.query(request), query ++ state.order->orderToQS) {
    | None => <Spinner />
    | Some(Error(title)) => <Alert variant=#Danger title />
    | Some(Ok(SearchTypes.Error(err))) =>
      <Alert
        title={err.message ++ " at " ++ string_of_int(Int32.to_int(err.position))} variant=#Danger
      />
    | Some(Ok(SearchTypes.Changes(items))) => {
        let changes = items.changes->Belt.List.toArray
        switch changes->Belt.Array.length {
        | 0 => <p> {"No changes matched"->str} </p>
        | _ =>
          <MStack>
            <MStackItem>
              <MCenteredContent> <ChangesTopPies store /> </MCenteredContent>
            </MStackItem>
            <MStackItem>
              <MCenteredContent> <Search.Filter store /> </MCenteredContent>
            </MStackItem>
            <MStackItem>
              <MCenteredContent> <ChangeList store changes /> </MCenteredContent>
            </MStackItem>
          </MStack>
        }
      }
    | Some(Ok(_)) => <Alert title={"Invalid response"} />
    }}
  </div>
}

let default = make
