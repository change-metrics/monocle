// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// The change component
//

open Prelude
open MLink

let complexicity = (change: SearchTypes.change) =>
  Int32.to_int(change.changed_files_count) +
  Int32.to_int(change.additions) +
  Int32.to_int(change.deletions)

module TaskData = {
  type t = SearchTypes.task_data

  module TaskTag = {
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
    let make = (~ps, ~name) =>
      switch ps {
      | "" => React.null
      | _ => {
          let label = name ++ ": " ++ ps
          <Patternfly.Label color={ps->getLabelColor}> {label} </Patternfly.Label>
        }
      }
  }

  module TaskScore = {
    @react.component
    let make = (~score) =>
      switch Int32.to_int(score) {
      | 0 => React.null
      | score => {
          let label = "Score: " ++ string_of_int(score)
          <Patternfly.Label> {label} </Patternfly.Label>
        }
      }
  }

  module TaskLink = {
    let getName = (td: SearchTypes.task_data) =>
      switch td.prefix {
      | "" => " " ++ td.url
      | prefix if Js.String.endsWith("#", prefix) => " " ++ prefix ++ td.tid
      | prefix => " " ++ prefix
      }

    @react.component
    let make = (~td: SearchTypes.task_data) =>
      switch td.url {
      // legacy hard-coded prefix, to be removed using a janitor process to update existing task datas.
      | url if Js.String.indexOf("show_bug.cgi", url) >= 0 =>
        <a href=url> <Patternfly.Icons.ExternalLinkAlt /> {(" rhbz#" ++ td.tid)->str} </a>
      | url => <a href=url> {td->getName->str} </a>
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
      <Patternfly.LabelGroup categoryName="Tags">
        {td.ttype->Belt.List.map(x => <TaskTag ttype={x} />)->Belt.List.toArray->React.array}
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

module Tags = {
  module Tag = {
    @react.component
    let make = (~tag: string) => {
      <Patternfly.Label color=#Grey> {tag} </Patternfly.Label>
    }
  }
  @react.component
  let make = (~tags: list<string>, ~withGroup: bool) => {
    let tags' =
      tags
      ->Belt.List.mapWithIndex((idx, tag) => <Tag key={string_of_int(idx)} tag />)
      ->Belt.List.toArray
      ->React.array
    withGroup
      ? <Patternfly.LabelGroup categoryName={"Tags"} numLabels={5}> {tags'} </Patternfly.LabelGroup>
      : {tags'}
  }
}

module Mergeable = {
  @react.component
  let make = (~state: string, ~mergeable: bool) =>
    switch state {
    | "MERGED" => React.null
    | "CLOSED" => React.null
    | _ =>
      mergeable
        ? React.null
        : <Patternfly.Label color=#Orange> {"Conflicting"->str} </Patternfly.Label>
    }
}

module FilterLink = {
  @react.component
  let make = (~store: Store.t, ~queryField: string, ~queryValue: string, ~name: string) => {
    let (state, _) = store
    let newFilter = queryField ++ ":\"" ++ queryValue ++ "\""
    let filter = Js.String.includes(newFilter, state.filter)
      ? state.filter
      : addQuery(state.filter, newFilter)
    let path = "changes"
    <MonoLink store filter path name />
  }
}

module ProjectLink = {
  @react.component
  let make = (~store, ~project) => {
    <span style={horizontalSpacing}>
      <FilterLink store queryField="repo" queryValue={project} name=project />
    </span>
  }
}

module BranchLink = {
  @react.component
  let make = (~store, ~branch) => {
    <span style={horizontalSpacing}>
      <FilterLink store queryField="branch" queryValue={branch} name=branch />
    </span>
  }
}

module ChangeLink = {
  @react.component
  let make = (~store: Store.t, ~id) => {
    let (state, _) = store
    <>
      {"("->str}
      <MLink.Direct link={"/" ++ state.index ++ "/change/" ++ id} name={"details"} />
      {")"->str}
    </>
  }
}

module AuthorLink = {
  @react.component
  let make = (~store: Store.t, ~title, ~author) => {
    <> {title->str} <FilterLink store queryField="author" queryValue={author} name={author} /> </>
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
    let (color, value) = draft
      ? (#Grey, "Draft")
      : switch state {
        | "OPEN" => (#Green, "Open")
        | "MERGED" => (#Blue, "Merged")
        | "CLOSED" => (#Purple, "Abandoned")
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

module TaskDatas = {
  @react.component
  let make = (~change: SearchTypes.change) => {
    switch change.task_data {
    | list{} => React.null
    | xs =>
      <MStack>
        {xs
        ->Belt.List.mapWithIndex((i, td) =>
          <MStackItem key={string_of_int(i)}> <TaskData td /> </MStackItem>
        )
        ->Belt.List.toArray
        ->React.array}
      </MStack>
    }
  }
}

module StatusButton = {
  @react.component
  let make = (
    ~store: Store.t,
    ~change: SearchTypes.change,
    ~status: HiddenChanges.changeStatus,
    ~dispatchChange: HiddenChanges.dispatch,
  ) => {
    let (hide, reveal) = dispatchChange
    let (tooltip, toast, button, action) = switch status {
    | Visible => (
        "Hide this change until it is updated",
        "Change hidden, check the settings to undo",
        `🛸`,
        hide,
      )
    | Updated => (
        "Hidden change got updated, click to hide again",
        "Change hidden again",
        `⚓`,
        hide,
      )
    | Hidden => ("Keep this change visible", "Hidden status removed", `🧐`, reveal)
    }
    let (_, dispatch) = store
    let onClick = _ => {
      toast->AddToast->dispatch
      change->action
    }
    <Patternfly.Tooltip content={tooltip}>
      <a onClick style={ReactDOM.Style.make(~paddingRight="5px", ~paddingLeft="5px", ())}>
        {button->str}
      </a>
    </Patternfly.Tooltip>
  }
}

module DataItem = {
  @react.component
  let make = (
    ~store: Store.t,
    ~change: SearchTypes.change,
    ~status: HiddenChanges.changeStatus,
    ~dispatchChange: HiddenChanges.dispatch,
  ) =>
    <DataListItemRow key={change.url}>
      <DataListCell>
        <Card isCompact={true}>
          <CardHeader>
            <span style={ReactDOM.Style.make(~width="100%", ())}>
              <State state={change.state} draft={change.draft} />
              <Mergeable state={change.state} mergeable={change.mergeable} />
              <ProjectLink store project={change.repository_fullname} />
              {list{"master", "main", "devel"}->elemText(change.target_branch)
                ? React.null
                : <> {"<"->str} <BranchLink store branch={change.target_branch} /> {">"->str} </>}
              <ExternalLink href={change.url} title={change.title} />
              <ChangeLink store id={change.change_id} />
              <StatusButton store change status dispatchChange />
              <span style={ReactDOM.Style.make(~float="right", ())}>
                {"Complexicity: "->str}
                <Badge isRead={true}> {change->complexicity->string_of_int->str} </Badge>
              </span>
            </span>
          </CardHeader>
          <CardBody>
            <div style={oneLineStyle}>
              <RelativeDate title="Created " date={change.created_at->getDate} />
              <AuthorLink store title=" by " author={change.author} />
              <RelativeDate title=", updated " date={change.updated_at->getDate} />
            </div>
            <Approvals withGroup={true} approvals={change.approval} />
            <Tags withGroup={true} tags={change.labels} />
            <TaskDatas change />
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
          <th role="columnheader"> {"Branch"->str} </th>
          <th role="columnheader"> {"Created"->str} </th>
          <th role="columnheader"> {"Updated"->str} </th>
          <th role="columnheader"> {"Size"->str} </th>
          <th role="columnheader"> {"Approvals / Tags / Tasks"->str} </th>
        </tr>
      </thead>
  }
  @react.component
  let make = (
    ~store: Store.t,
    ~change: SearchTypes.change,
    ~status: HiddenChanges.changeStatus,
    ~dispatchChange: HiddenChanges.dispatch,
  ) =>
    <tr role="row">
      <td role="cell">
        <StatusButton store change status dispatchChange />
        <ExternalLink href={change.url} title={change.title} />
        // show details button, currently commented as it looks a bit noisy...
        // <ChangeLink store id={change.change_id} title={change.title} />
      </td>
      <td role="cell">
        <div style={oneLineStyle}>
          <State state={change.state} draft={change.draft} />
          <Mergeable state={change.state} mergeable={change.mergeable} />
        </div>
      </td>
      <td role="cell"> <AuthorLink store title="" author={change.author} /> </td>
      <td role="cell"> <ProjectLink store project={change.repository_fullname} /> </td>
      <td role="cell"> <BranchLink store branch={change.target_branch} /> </td>
      <td role="cell"> <RelativeDate title="" date={change.created_at->getDate} /> </td>
      <td role="cell"> <RelativeDate title="" date={change.updated_at->getDate} /> </td>
      <td role="cell">
        <Badge isRead={true}> {change->complexicity->string_of_int->str} </Badge>
      </td>
      <td role="cell">
        <div> <Approvals withGroup={true} approvals={change.approval} /> </div>
        <div> <Tags withGroup={true} tags={change.labels} /> </div>
        <div> <TaskDatas change /> </div>
      </td>
    </tr>
}

module Table = {
  @react.component
  let make = (~store: Store.t, ~changes: list<SearchTypes.change>) => {
    let (changesArray, paginate) = changes->Belt.List.toArray->usePagination
    let (state, _) = store
    let (changesFiltered, dispatchChange) = HiddenChanges.use(state.dexie, changesArray)
    <>
      {paginate}
      <table className="pf-c-table pf-m-compact pf-m-grid-md" role="grid">
        <RowItem.Head />
        <tbody role="rowgroup">
          {changesFiltered
          ->Belt.Array.mapWithIndex((idx, (status, change)) =>
            status != HiddenChanges.Hidden
              ? <RowItem key={string_of_int(idx)} store change status dispatchChange />
              : React.null
          )
          ->React.array}
        </tbody>
      </table>
    </>
  }
}
