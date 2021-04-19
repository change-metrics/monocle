// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// The changes view
//
open Prelude

// Binding for existing components
module CChangesAuthorsPie = {
  @react.component @module("./changes_authors_pie.js")
  external make: (~index: string) => React.element = "default"
}

module CReposPie = {
  @react.component @module("./repos_pie.js")
  external make: (~index: string) => React.element = "default"
}

module CApprovalsPie = {
  @react.component @module("./approvals.js")
  external make: (~index: string) => React.element = "CApprovalsPie"
}

module CLastChangesNG = {
  @react.component @module("./changes")
  external make: (~index: string, ~showComplexityGraph: bool) => React.element = "CLastChangesNG"
}

// The ChangesView components
module StatsToggle = {
  let boxStyle = ReactDOM.Style.make(
    ~backgroundColor="#dbecff6b",
    ~textAlign="center",
    ~cursor="pointer",
    (),
  )
  let pStyle = ReactDOM.Style.make(~marginTop="0rem", ~marginBottom="0rem", ~fontWeight="bold", ())

  @react.component
  let make = (~showPies: bool, ~setShowPies) => {
    let statMessage = showPies ? "Collapse stats" : "Display stats"
    <MSimpleCard style=boxStyle onClick={_ => setShowPies(x => !x)}>
      <p style=pStyle> {statMessage->React.string} </p>
    </MSimpleCard>
  }
}

module Pies = {
  @react.component
  let make = (~index: string) =>
    <MGrid>
      <MGridItem> <CChangesAuthorsPie index /> </MGridItem>
      <MGridItem> <CReposPie index /> </MGridItem>
      <MGridItem> <CApprovalsPie index /> </MGridItem>
    </MGrid>
}

@react.component
let make = (~index: string) => {
  let (showPies, setShowPies) = React.useState(_ => false)
  <MStack>
    <MStackItem> <StatsToggle showPies setShowPies /> </MStackItem>
    {showPies->maybeRender(<MStackItem> <Pies index /> </MStackItem>)}
    <MStackItem> <CLastChangesNG index showComplexityGraph={showPies} /> </MStackItem>
  </MStack>
}

let default = make
