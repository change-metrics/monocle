// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// The change component
//

open Prelude

let getDate = (ts: option<TimestampTypes.timestamp>): Js.Date.t =>
  ts->Belt.Option.getExn->Belt.Option.getExn

module TaskData = {
  type t = TaskDataTypes.new_task_data

  module TaskType = {
    @react.component
    let make = (~ttype: string) => <Patternfly.Label> {ttype->str} </Patternfly.Label>
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
      let label = "Score: " ++ string_of_int(Int32.to_int(score))
      <Patternfly.Label> {label} </Patternfly.Label>
    }
  }

  module TaskLink = {
    @react.component
    let make = (~td: TaskDataTypes.new_task_data) =>
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
      <Patternfly.LabelGroup categoryName="Type">
        {td.ttype->Belt.List.map(x => <TaskType ttype={x} />)->Belt.List.toArray->React.array}
      </Patternfly.LabelGroup>
    </>
  }
}

module Approvals = {
  module Label = {
    @react.component
    let make = (~approval: string) => {
      let regex = %re("/.*-.$/")
      let color = switch approval {
      | "CHANGES_REQUESTED"
      | "REVIEW_REQUIRED" =>
        #Orange
      | _ if regex->Js.Re.test_(approval) => #Red
      | _ if Js.String.includes("+0", approval) => #Grey
      | _ => #Green
      }
      <Patternfly.Label color> {approval} </Patternfly.Label>
    }
  }
  @react.component
  let make = (~approvals: list<string>, ~withGroup: bool) => {
    let labels =
      approvals
      ->Belt.List.mapWithIndex((idx, approval) => <Label key={string_of_int(idx)} approval />)
      ->Belt.List.toArray
      ->React.array
    withGroup
      ? <Patternfly.LabelGroup categoryName={"Approvals"} numLabels={5}>
          {labels}
        </Patternfly.LabelGroup>
      : {labels}
  }
}

let horizontalSpacing = ReactDOM.Style.make(~paddingLeft="5px", ~paddingRight="5px", ())

module ExternalLink = {
  @react.component
  let make = (~href) =>
    <a href target="_blank" rel="noopener noreferre" style={horizontalSpacing}> {`🔗`->str} </a>
}

module ProjectLink = {
  @react.component
  let make = (~index, ~project, ~branch) => {
    let name =
      list{"master", "main", "devel"}->elemText(branch) ? project : project ++ "<" ++ branch ++ ">"
    <span style={horizontalSpacing}>
      {"["->str}
      <a
        style={ReactDOM.Style.make(~whiteSpace="nowrap", ())}
        href={"/" ++ index ++ "/changes?project=" ++ project}>
        {name->str}
      </a>
      {"]"->str}
    </span>
  }
}
module ChangeLink = {
  @react.component
  let make = (~index, ~id, ~title) => <a href={"/" ++ index ++ "/change/" ++ id}> {title->str} </a>
}

module AuthorLink = {
  @react.component
  let make = (~index, ~title, ~author) => {
    <> {title->str} <a href={"/" ++ index ++ "/changes?author=" ++ author}> {author->str} </a> </>
  }
}

module RelativeDate = {
  @react.component
  let make = (~title, ~date) => {
    let dateStr = date->momentFromNow
    <> {title->str} {dateStr->str} </>
  }
}

module State = {
  @react.component
  let make = (~state) => <Label> {state->str} </Label>
}

module DataItem = {
  let oneLineStyle = ReactDOM.Style.make(
    ~overflow="hidden",
    ~textOverflow="ellipsis",
    ~whiteSpace="nowrap",
    (),
  )

  @react.component
  let make = (~index: string, ~change: SearchTypes.change) =>
    <DataListItemRow key={change.url}>
      <DataListCell>
        <Card isCompact={true}>
          <CardHeader>
            <State state={change.state} />
            <ExternalLink href={change.url} />
            <ProjectLink index project={change.repository_fullname} branch={change.target_branch} />
            <span style={ReactDOM.Style.make(~textAlign="right", ~width="100%", ())}>
              {"Complexicity: "->str} <Badge isRead={true}> {42->string_of_int->str} </Badge>
            </span>
          </CardHeader>
          <CardBody>
            <div style={oneLineStyle}>
              {"Title: "->str} <ChangeLink index id={change.change_id} title={change.title} />
            </div>
            <div style={oneLineStyle}>
              <RelativeDate title="Created " date={change.created_at->getDate} />
              <AuthorLink index title=" by " author={change.author} />
              <RelativeDate title=", updated " date={change.updated_at->getDate} />
            </div>
            <Approvals withGroup={true} approvals={change.approval} />
            {switch change.task_data {
            | list{} => React.null
            | xs => xs->Belt.List.map(td => <TaskData td />)->Belt.List.toArray->React.array
            }}
          </CardBody>
        </Card>
      </DataListCell>
    </DataListItemRow>
}
