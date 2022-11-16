// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// A designer app to help style the components without the backend
//

%%raw(`
import '@patternfly/react-core/dist/styles/base.css'
import '@patternfly/react-styles/css/components/Table/table.css'
import './index.css'
`)

open Prelude

module Fixture = {
  @module external changeJson: Js.Dict.t<Js.Json.t> = "../../schemas/monocle/change.json"
  let change: SearchTypes.change = SearchBs.decode_change(changeJson)

  @module
  external searchFieldsJson: Js.Dict.t<Js.Json.t> = "../../schemas/monocle/search_fields.json"
  let fields: SearchTypes.fields_response = SearchBs.decode_fields_response(searchFieldsJson)
}

let hiddenDispatchChange = (c => Js.log2("Hidding", c), c => Js.log2("Revealing", c))
let pinnedDispatchChange = (c => Js.log2("Pin", c), c => Js.log2("Unpin", c))
let maskedDispatchChange = (c => Js.log2("Mask", c), c => Js.log2("Unmask", c))

let hiddenStatus = HiddenChanges.Visible
let pinnedStatus = PinnedChanges.Unpinned
let maskedStatus = MaskedChanges.Masked

module Table = {
  @react.component
  let make = (~store: Store.t, ~changes: list<SearchTypes.change>) => {
    let (changesArray, paginate) = changes->Belt.List.toArray->usePagination
    let (state, _) = store
    let (hiddenChanges, hiddenDispatchChange) = HiddenChanges.use(state.dexie, changesArray)
    let (pinnedChanges, pinnedDispatchChange) = PinnedChanges.use(state.dexie, changesArray)
    let (maskedChanges, maskedDispatchChange) = MaskedChanges.use(state.dexie, changesArray)
    <>
      {paginate}
      <table className="pf-c-table pf-m-compact pf-m-grid-md" role="grid">
        <Change.RowItem.Head />
        <tbody role="rowgroup">
          {hiddenChanges
          ->Belt.Array.mapWithIndex((idx, (hiddenStatus, change)) =>
            hiddenStatus != HiddenChanges.Hidden
              ? <Change.RowItem
                  key={string_of_int(idx)}
                  store
                  change
                  hiddenStatus
                  hiddenDispatchChange
                  pinnedStatus={PinnedChanges.simpleGetStatus(pinnedChanges, change)}
                  pinnedDispatchChange
                  maskedStatus={MaskedChanges.simpleGetStatus(maskedChanges, change)}
                  maskedDispatchChange
                />
              : React.null
          )
          ->React.array}
        </tbody>
      </table>
    </>
  }
}

module App = {
  @react.component
  let make = () => {
    let fakeAbout: ConfigTypes.about = {
      version: "1.2.3",
      links: list{},
      auth: ConfigTypes.Auth_config(ConfigTypes.default_about_auth_config()),
    }
    let store = Store.use("test", fakeAbout)
    <>
      {[
        ("title", <h2> {"Monocle designer mode"->str} </h2>),
        (
          "change",
          <div className="container">
            <Change.DataItem
              store
              change={Fixture.change}
              hiddenStatus
              hiddenDispatchChange
              pinnedStatus
              pinnedDispatchChange
              maskedStatus
              maskedDispatchChange
            />
          </div>,
        ),
        (
          "table",
          <Table
            store
            changes={Belt.List.make(200, Fixture.change)->Belt.List.mapWithIndex((idx, change) => {
              ...change,
              title: "test " ++ string_of_int(idx),
            })}
          />,
        ),
        (
          "changeList",
          <NChangeView.ChangeList
            store
            changes={Belt.List.make(100, Fixture.change)
            ->Belt.List.map(c => (hiddenStatus, c))
            ->Belt.List.toArray}
            hiddenDispatchChange
            pinnedChanges=[]
            pinnedDispatchChange
            maskedChanges=[]
            maskedDispatchChange
          />,
        ),
        (
          "search help",
          <>
            <div className="container"> <HelpSearch.Tooltip store /> {"test"->str} </div>
            <div className="container"> <HelpSearch.Content fields={Fixture.fields.fields} /> </div>
          </>,
        ),
      ]
      ->Belt.Array.map(((key, v)) => <span key> {v} <hr /> </span>)
      ->React.array}
    </>
  }
}

switch ReactDOM.querySelector("#root") {
| Some(root) => ReactDOM.render(<App />, root)
| None => Js.log("Can't find #root element!")
}
