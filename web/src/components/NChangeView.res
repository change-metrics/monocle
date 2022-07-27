open Prelude

module PieWithLegend = {
  type entry = {
    key: string,
    doc_count: int,
  }
  type t = {
    items: array<entry>,
    total_hits: int,
  }
  type named_palette_t = {
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

  let getColorFromNamedPalette = (label: string, namedPalette: named_palette_t): option<string> => {
    switch label {
    | "APPROVED" => namedPalette["APPROVED"]->Some
    | "CHANGES_REQUESTED" => namedPalette["CHANGES_REQUESTED"]->Some
    | "COMMENTED" => namedPalette["COMMENTED"]->Some
    | "Code-Review+1" => namedPalette["Code-Review+1"]->Some
    | "Code-Review+2" => namedPalette["Code-Review+2"]->Some
    | "Code-Review-1" => namedPalette["Code-Review-1"]->Some
    | "Code-Review-2" => namedPalette["Code-Review-2"]->Some
    | "DISMISSED" => namedPalette["DISMISSED"]->Some
    | "Workflow+1" => namedPalette["Workflow+1"]->Some
    | "Workflow-1" => namedPalette["Workflow-1"]->Some
    | _ => None
    }
  }

  let defaultPalette = [
    "#247ba0",
    "#70c1b3",
    "#b2dbbf",
    "#f3ffbd",
    "#ff1654",
    "#247ba0",
    "#70c1b3",
    "#b2dbbf",
    "#f3ffbd",
    "#ff1654",
    "#b2dbbf",
  ]

  let getColorFromPalette = (index: int, label: string, namedPaletteM: option<named_palette_t>) => {
    let getFromDefaultPalette = (i: int) =>
      switch defaultPalette->Belt.Array.get(i) {
      | Some(color) => color
      | None => "black"
      }
    switch namedPaletteM {
    | Some(namedPalette) =>
      switch getColorFromNamedPalette(label, namedPalette) {
      | Some(color) => color
      | None => getFromDefaultPalette(index)
      }
    | None => getFromDefaultPalette(index)
    }
  }

  module PieChart = {
    @react.component @module("./chartjs.jsx")
    external make: (
      ~data: t,
      ~palette: array<string>,
      ~namedPalette: named_palette_t=?,
      ~handleClick: (~value: string) => unit,
      ~other_label: string=?,
    ) => React.element = "PieChart"
  }

  module PieChartLegend = {
    @react.component
    let make = (
      ~data: t,
      ~namedPalette: option<named_palette_t>,
      ~handleClick: (~value: string) => unit,
    ) => {
      data.items
      ->Belt.Array.mapWithIndex((i, e) =>
        <div key={e.key}>
          <span
            style={ReactDOM.Style.make(
              ~backgroundColor={getColorFromPalette(i, e.key, namedPalette)},
              ~width="10px",
              ~height="10px",
              ~display="inline-block",
              ~cursor="pointer",
              (),
            )}
          />
          <span> {" "->str} </span>
          <span
            key={e.key}
            onClick={_ => handleClick(~value=e.key)}
            style={ReactDOM.Style.make(~cursor="pointer", ())}>
            {e.key->str}
          </span>
        </div>
      )
      ->React.array
    }
  }

  @react.component
  let make = (
    ~data: t,
    ~title: string,
    ~handleClick: (~value: string) => unit,
    ~namedPalette: option<named_palette_t>=?,
    ~other_label: option<string>=?,
  ) => {
    <React.Fragment>
      <Patternfly.Card isCompact={true}>
        <Patternfly.CardTitle>
          <Patternfly.Title headingLevel=#H3> {title->str} </Patternfly.Title>
        </Patternfly.CardTitle>
        <Patternfly.CardBody>
          <PieChart data palette={defaultPalette} handleClick ?namedPalette ?other_label />
          <PieChartLegend data namedPalette handleClick />
        </Patternfly.CardBody>
      </Patternfly.Card>
    </React.Fragment>
  }
}

module ChangeList = {
  @react.component
  let make = (
    ~store: Store.t,
    ~changes: HiddenChanges.changeArray,
    ~dispatchChange: HiddenChanges.dispatch,
    ~disableHiddenChange: option<bool>=?,
  ) => {
    let (toggle, isChangeVisible) = switch disableHiddenChange {
    | Some(true) => (React.null, _ => true)
    | _ => HiddenChanges.useToggle()
    }
    let (changesArray, paginate) = usePagination(changes)
    <MStack>
      <MStackItem> {toggle} </MStackItem>
      <MStackItem> {paginate} </MStackItem>
      <MStackItem>
        <Patternfly.DataList isCompact={true}>
          {changesArray
          ->Belt.Array.map(((status, change)) =>
            isChangeVisible(status)
              ? <Change.DataItem store key={change.change_id} change status dispatchChange />
              : React.null
          )
          ->React.array}
        </Patternfly.DataList>
      </MStackItem>
    </MStack>
  }
}

module ChangesTopPies = {
  @react.component
  let make = (~store, ~extraQuery: option<string>=?, ~hideAuthors: option<bool>=?) => {
    let (state, dispatch) = store
    let qtype = SearchTypes.Query_changes_tops
    let baseRequest = Store.mkSearchRequest(state, qtype)
    let query = addQuery(baseRequest.query, state.filter)
    let request = {
      ...baseRequest,
      query: switch extraQuery {
      | Some(contextQ) => addQuery(query, contextQ)
      | None => query
      },
      limit: 10->Int32.of_int,
    }
    let getEntry = (e: MetricTypes.term_count_int): PieWithLegend.entry => {
      doc_count: e.count->Int32.to_int,
      key: e.term,
    }
    let adapt = (
      elms: MetricTypes.terms_count_int,
      kf: PieWithLegend.entry => bool,
    ): PieWithLegend.t => {
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
      let base = readWindowLocationPathname() ++ "?"
      let query = switch state.query {
      | "" => ""
      | query => "q=" ++ query ++ "&"
      }
      let href = base ++ query ++ "f=" ++ filter
      filter->Store.Store.SetFilter->dispatch
      href->RescriptReactRouter.push
    }
    let tokenM = state->Store.Store.getAuthenticatedUserJWT
    <QueryRender
      request
      tokenM
      trigger={query ++ extraQuery->Belt.Option.getWithDefault("")}
      render={resp =>
        switch resp {
        | SearchTypes.Changes_tops(items) =>
          <MExpandablePanel
            title={"Show changes pie charts"} stateControler={(state.changes_pies_panel, tee)}>
            <Layout.Bullseye>
              <Layout.Flex>
                {maybeHide(
                  hideAuthors,
                  <Layout.FlexItem>
                    <PieWithLegend
                      data={items.authors->Belt.Option.getExn->adapt(_ => true)}
                      title={"Changes per author"}
                      handleClick={handlePieClick(state, dispatch, ~field="author")}
                    />
                  </Layout.FlexItem>,
                )}
                <Layout.FlexItem>
                  <PieWithLegend
                    data={items.repos->Belt.Option.getExn->adapt(_ => true)}
                    title={"Changes per repository"}
                    handleClick={handlePieClick(state, dispatch, ~field="repo")}
                  />
                </Layout.FlexItem>
                <Layout.FlexItem>
                  <PieWithLegend
                    data={items.approvals
                    ->Belt.Option.getExn
                    ->adapt(e => ignoredApproval->Belt.Array.some(e' => e' != e.key))}
                    title={"Changes per approval"}
                    handleClick={handlePieClick(state, dispatch, ~field="approval")}
                    namedPalette={approvals_palette}
                  />
                </Layout.FlexItem>
              </Layout.Flex>
            </Layout.Bullseye>
          </MExpandablePanel>
        | _ => <Alert title={"Invalid response"} />
        }}
    />
  }
}

module View = {
  @react.component
  let make = (~store: Store.t, ~changesAll, ~extraQuery, ~hideAuthors, ~disableHiddenChange) => {
    let (state, _) = store
    let (changes, dispatchChange) = HiddenChanges.use(state.dexie, changesAll)
    switch changes->Belt.Array.length {
    | 0 =>
      <MStack>
        <MStackItem> <Search.Filter store /> </MStackItem>
        <MStackItem> <p> {"No changes matched"->str} </p> </MStackItem>
      </MStack>
    | _ =>
      <MStack>
        <MStackItem> <ChangesTopPies store ?extraQuery ?hideAuthors /> </MStackItem>
        <MStackItem> <Search.Filter store /> </MStackItem>
        <MStackItem> <ChangeList store changes dispatchChange ?disableHiddenChange /> </MStackItem>
      </MStack>
    }
  }
}

@react.component
let make = (
  ~store: Store.t,
  ~extraQuery: option<string>=?,
  ~hideAuthors: option<bool>=?,
  ~disableHiddenChange: option<bool>=?,
) => {
  let (state, _) = store
  let baseRequest = Store.mkSearchRequest(state, SearchTypes.Query_change)
  let query = addQuery(baseRequest.query, state.filter)
  let request = {
    ...baseRequest,
    query: switch extraQuery {
    | Some(contextQ) => addQuery(query, contextQ)
    | None => query
    },
    limit: 256->Int32.of_int,
  }
  let trigger = query ++ state.order->orderToQS ++ extraQuery->Belt.Option.getWithDefault("")
  let title = "Changes"
  let tooltip_content = "This shows the list of changes"
  let icon = <Patternfly.Icons.Integration />
  let tokenM = state->Store.Store.getAuthenticatedUserJWT
  let match = resp =>
    switch resp {
    | SearchTypes.Changes(items) => items.changes->Some
    | _ => None
    }
  let childrenBuilder = changes =>
    <View
      store changesAll={changes->Belt.List.toArray} extraQuery hideAuthors disableHiddenChange
    />

  <QueryRenderCard request tokenM trigger title tooltip_content icon match childrenBuilder />
}

let default = make
