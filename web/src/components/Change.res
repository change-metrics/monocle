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
    severity: string,
    priority: string,
    score: string,
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

  module TaskPS = {
    let getLabelColor = (level: string) =>
      switch level {
      | "urgent" => #Purple
      | "high" => #Red
      | "medium" => #Orange
      | "low" => #Green
      | _ => #Green
      }
    @react.component
    let make = (~ps, ~name) => {
      let label = name ++ ": " ++ ps
      <Patternfly.Label color={ps->getLabelColor}> {label} </Patternfly.Label>
    }
  }

  module TaskScore = {
    @react.component
    let make = (~score) => {
      let label = "Score: " ++ score
      <Patternfly.Label> {label} </Patternfly.Label>
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
    <>
      {"Task: "->str}
      <TaskLink td />
      <TaskPS ps=td.priority name="Priority" />
      <TaskPS ps=td.severity name="Severity" />
      <TaskScore score=td.score />
      <Patternfly.LabelGroup>
        <Patternfly.LabelGroup categoryName="Type">
          {td.ttype->Belt.Array.map(x => <TaskType ttype={x} />)}
        </Patternfly.LabelGroup>
      </Patternfly.LabelGroup>
    </>
  }
}
