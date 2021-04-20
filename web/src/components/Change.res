// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// The change component
//

open Prelude

module TaskData = {
  type t = {
    title: string,
    url: string,
    ttype: array<string>,
    tid: string,
  }
  module TaskType = {
    @react.component
    let make = (~ttype: string) =>
      switch ttype {
      | "FutureFeature" =>
        <Patternfly.Label icon={<Patternfly.Icons.Enhancement />}> {"RFE"->str} </Patternfly.Label>
      | x => <Patternfly.Label> {x->str} </Patternfly.Label>
      }
  }
  module TaskLink = {
    @react.component
    let make = (~td) =>
      switch td.url {
      | url if Js.String.indexOf("show_bug.cgi", url) >= 0 =>
        <a href=url> <Patternfly.Icons.ExternalLinkAlt /> {(" rhbz#" ++ td.tid)->str} </a>
      | url => <a href=url> {url->str} </a>
      }
  }

  @react.component
  let make = (~td: t) => {
    <p>
      {"Task: "->str}
      <TaskLink td />
      <Patternfly.LabelGroup>
        <Patternfly.LabelGroup categoryName="Type">
          {td.ttype->Belt.Array.map(x => <TaskType ttype={x} />)}
        </Patternfly.LabelGroup>
      </Patternfly.LabelGroup>
    </p>
  }
}
