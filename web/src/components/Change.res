// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// The change component
//

open Prelude

let getDate = (ts: option<TimestampTypes.timestamp>): Js.Date.t =>
  ts->Belt.Option.getExn->Belt.Option.getExn

let complexicity = (change: SearchTypes.change) =>
  Int32.to_int(change.changed_files_count) + Int32.to_int(change.additions) + Int32.to_int(change.deletions)

module TaskData = {
  type t = TaskDataTypes.task_data

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
    let make = (~td: TaskDataTypes.task_data) =>
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

module Mergeable = {
  @react.component
  let make = (~mergeable: bool) =>
    <Patternfly.Label color={mergeable ? #Green : #Orange}>
        {(mergeable ? "Mergeable" : "Conflicting")->str}
    </Patternfly.Label>
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
      <Link
        style={ReactDOM.Style.make(~whiteSpace="nowrap", ())}
        _to={"/" ++ index ++ "/changes?project=" ++ project}>
        {name->str}
      </Link>
      {"]"->str}
    </span>
  }
}
module ChangeLink = {
  @react.component
  let make = (~index, ~id, ~title) => <Link _to={"/" ++ index ++ "/change/" ++ id}> {title->str} </Link>
}

module AuthorLink = {
  @react.component
  let make = (~index, ~title, ~author) => {
    <> {title->str} <Link _to={"/" ++ index ++ "/changes?author=" ++ author}> {author->str} </Link> </>
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
  let make = (~state, ~draft) => {
    let (color, value) = draft ? (#Grey, "Draft") : switch state {
      | "OPEN" => (#Green, "Open")
      | "Merged" => (#Blue, "Merged")
      | "Closed" => (#Purple, "Closed")
      | _ => (#Red, state)
    }
    <Label color> {value->str} </Label>
  }
}

let oneLineStyle = ReactDOM.Style.make(
  ~overflow="hidden",
  ~textOverflow="ellipsis",
  ~whiteSpace="nowrap",
  (),
)

module DataItem = {
  @react.component
  let make = (~index: string, ~change: SearchTypes.change) =>
    <DataListItemRow key={change.url}>
      <DataListCell>
        <Card isCompact={true}>
          <CardHeader>
            <State state={change.state} draft={change.draft} />
            <Mergeable mergeable={change.mergeable} />
            <ExternalLink href={change.url} />
            <ProjectLink index project={change.repository_fullname} branch={change.target_branch} />
            <span style={ReactDOM.Style.make(~textAlign="right", ~width="100%", ())}>
              {"Complexicity: "->str} <Badge isRead={true}> {change->complexicity->string_of_int->str} </Badge>
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

module RowItem = {
  module Head = {
    @react.component
    let make = () =>
      <thead>
        <tr role="row">
          <th role="columnheader"> {"Title"->str} </th>
          <th role="columnheader"> {"Status"->str} </th>
          <th role="columnheader"> {"Owner"->str} </th>
          <th role="columnheader"> {"Repo"->str} </th>
          <th role="columnheader"> {"Created"->str} </th>
          <th role="columnheader"> {"Updated"->str} </th>
          <th role="columnheader"> {"Size"->str} </th>
          <th role="columnheader"> {"Approvals"->str} </th>
        </tr>
      </thead>
  }
  @react.component
  let make = (~index: string, ~change: SearchTypes.change) =>
    <tr role="row">
      <td role="cell"> <ChangeLink index id={change.change_id} title={change.title} /> </td>
      <td role="cell">
        <div style={oneLineStyle}>
          <State state={change.state} draft={change.draft} /> <Mergeable mergeable={change.mergeable} />
        </div>
      </td>
      <td role="cell"> <AuthorLink index title="" author={change.author} /> </td>
      <td role="cell">
        <ProjectLink index project={change.repository_fullname} branch={change.target_branch} />
      </td>
      <td role="cell"> <RelativeDate title="" date={change.created_at->getDate} /> </td>
      <td role="cell"> <RelativeDate title="" date={change.updated_at->getDate} /> </td>
      <td role="cell"> <Badge isRead={true}> {change->complexicity->string_of_int->str} </Badge> </td>
      <td role="cell"> <Approvals withGroup={false} approvals={change.approval} /> </td>
    </tr>
}

module Table = {
  @react.component
  let make = (~index: string, ~changes: list<SearchTypes.change>) =>
    <table className="pf-c-table pf-m-compact pf-m-grid-md" role="grid">
      <RowItem.Head />
      <tbody role="rowgroup">
        {changes
        ->Belt.List.mapWithIndex((idx, change) => <RowItem key={string_of_int(idx)} index change />)
        ->Belt.List.toArray
        ->React.array}
      </tbody>
    </table>
}
