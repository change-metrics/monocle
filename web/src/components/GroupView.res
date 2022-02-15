// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// The user group view component
//

open Prelude

module HistoBox = {
  @react.component
  let make = (~bucket: UserGroupTypes.review_histo) => {
    let count = bucket.count->Int32.to_int->Belt.Int.toFloat
    let countStr = switch bucket.count->Int32.to_int {
    | 0 => ""
    | x => x->string_of_int
    }
    let alpha = Js.Math.unsafe_round(255.0 *. Js.Math.log10(1.0 +. count))
    let green = 100 + Js.Math.unsafe_round(155.0 *. Js.Math.log10(1.0 +. count))
    let style = ReactDOM.Style.make(
      ~width="20px",
      ~height="20px",
      ~display="inline-block",
      ~border="1px solid black",
      ~margin="2px",
      ~borderRadius="5px",
      ~overflow="hidden",
      ~textAlign="center",
      ~backgroundColor="rgba(0, " ++ string_of_int(green) ++ ", 0, " ++ string_of_int(alpha) ++ ")",
      (),
    )
    let date = bucket.date->Int64.to_float->Js.Date.fromFloat->Js.Date.toDateString
    <Tooltip content={date->str}> <span style> {countStr->str} </span> </Tooltip>
  }
}

module RowItem = {
  module Head = {
    @react.component
    let make = () =>
      <thead>
        <tr role="row">
          <th role="columnheader"> {"Member"->str} </th>
          <Tooltip content={"The ratio between change created and change reviewed"}>
            <th role="columnheader"> {"Change / Review"->str} </th>
          </Tooltip>
          <th role="columnheader"> {"Daily review activity"->str} </th>
        </tr>
      </thead>
  }
  @react.component
  let make = (~user: UserGroupTypes.user_stat) => {
    let stat = user.stat->Belt.Option.getExn
    <tr role="row">
      <td role="cell"> {user.name->str} </td>
      <td role="cell">
        {stat.change_review_ratio >= 0.0
          ? <Canvas.Dom
              width=100
              height=20
              onDraw={Canvas.drawScale(stat.change_review_ratio->Js.Math.unsafe_round)}
            />
          : React.null}
      </td>
      <td role="cell">
        <Layout.Grid>
          <Layout.GridItem md=Column._1> {"Commit: "->str} </Layout.GridItem>
          <Layout.GridItem md=Column._11>
            {stat.commit_histo
            ->Belt.List.mapWithIndex((index, bucket) =>
              <HistoBox bucket key={index->string_of_int} />
            )
            ->Belt.List.toArray
            ->React.array}
          </Layout.GridItem>
          <Layout.GridItem md=Column._1> {"Review: "->str} </Layout.GridItem>
          <Layout.GridItem md=Column._11>
            {stat.review_histo
            ->Belt.List.mapWithIndex((index, bucket) =>
              <HistoBox bucket key={index->string_of_int} />
            )
            ->Belt.List.toArray
            ->React.array}
          </Layout.GridItem>
        </Layout.Grid>
      </td>
    </tr>
  }
}

module GroupTable = {
  @react.component
  let make = (~group: UserGroupTypes.get_response) => {
    <table className="pf-c-table pf-m-compact pf-m-grid-md" role="grid">
      <RowItem.Head />
      <tbody role="rowgroup">
        {group.users
        ->Belt.List.mapWithIndex((idx, user) => <RowItem key={string_of_int(idx)} user />)
        ->Belt.List.toArray
        ->React.array}
      </tbody>
    </table>
  }
}

@react.component
let make = (~group: string, ~store: Store.t) => {
  let (state, _) = store
  let groupName = group->Js.Global.decodeURIComponent
  let request: UserGroupTypes.get_request = {
    index: state.index,
    name: groupName,
    query: state.query,
  }
  let title = groupName
  let tooltip_content =
    "This shows for each member " ++
    "the ratio between changes created " ++ "and changes reviewed as well as daily activity charts"
  let icon = <Patternfly.Icons.UsersAlt />
  <MCenteredContent>
    <MonoCard title tooltip_content icon>
      <NetworkRender
        get={() => WebApi.UserGroup.get(request)}
        trigger={state.query}
        render={group => <GroupTable group />}
      />
    </MonoCard>
  </MCenteredContent>
}
let default = make
