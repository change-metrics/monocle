// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// The user group view component
//

open Prelude

// module HistoBox = {
//   @react.component
//   let make = (~bucket: UserGroupTypes.review_histo) => {
//     let count = bucket.count->Int32.to_int->Belt.Int.toFloat
//     let countStr = switch bucket.count->Int32.to_int {
//     | 0 => ""
//     | x => x->string_of_int
//     }
//     let alpha = Js.Math.unsafe_round(255.0 *. Js.Math.log10(1.0 +. count))
//     let green = 100 + Js.Math.unsafe_round(155.0 *. Js.Math.log10(1.0 +. count))
//     let style = ReactDOM.Style.make(
//       ~width="20px",
//       ~height="20px",
//       ~display="inline-block",
//       ~border="1px solid black",
//       ~margin="2px",
//       ~borderRadius="5px",
//       ~overflow="hidden",
//       ~textAlign="center",
//       ~backgroundColor="rgba(0, " ++ string_of_int(green) ++ ", 0, " ++ string_of_int(alpha) ++ ")",
//       (),
//     )
//     let date = bucket.date->Int64.to_float->Js.Date.fromFloat->Js.Date.toDateString
//     <Tooltip content={date->str}> <span style> {countStr->str} </span> </Tooltip>
//   }
// }

// module RowItem = {
//   module Head = {
//     @react.component
//     let make = () =>
//       <thead>
//         <tr role="row">
//           <th role="columnheader"> {"Member"->str} </th>
//           <Tooltip content={"The ratio between change created and change reviewed"}>
//             <th role="columnheader"> {"Change / Review"->str} </th>
//           </Tooltip>
//           <th role="columnheader"> {"Daily review activity"->str} </th>
//         </tr>
//       </thead>
//   }
//   @react.component
//   let make = (~store: Store.t, ~user: UserGroupTypes.user_stat) => {
//     let (state, dispatch) = store
//     let stat = user.stat->Belt.Option.getExn
//     let link = "/" ++ state.index ++ "/author/" ++ user.name->Js.Global.encodeURIComponent
//     <tr role="row">
//       <td role="cell">
//         {<a
//           onClick={e => {
//             e->ReactEvent.Mouse.preventDefault
//             ChangeActivity->SetAuthorScopedTab->dispatch
//             link->RescriptReactRouter.push
//           }}>
//           {user.name->str}
//         </a>}
//       </td>
//       <td role="cell">
//         {stat.change_review_ratio >= 0.0
//           ? <Canvas.Dom
//               width=100
//               height=20
//               onDraw={Canvas.drawScale(stat.change_review_ratio->Js.Math.unsafe_round)}
//             />
//           : React.null}
//       </td>
//       <td role="cell">
//         <Layout.Grid>
//           <Layout.GridItem md=Column._1> {"Commit: "->str} </Layout.GridItem>
//           <Layout.GridItem md=Column._11>
//             {stat.commit_histo
//             ->Belt.List.mapWithIndex((index, bucket) =>
//               <HistoBox bucket key={index->string_of_int} />
//             )
//             ->Belt.List.toArray
//             ->React.array}
//           </Layout.GridItem>
//           <Layout.GridItem md=Column._1> {"Review: "->str} </Layout.GridItem>
//           <Layout.GridItem md=Column._11>
//             {stat.review_histo
//             ->Belt.List.mapWithIndex((index, bucket) =>
//               <HistoBox bucket key={index->string_of_int} />
//             )
//             ->Belt.List.toArray
//             ->React.array}
//           </Layout.GridItem>
//         </Layout.Grid>
//       </td>
//     </tr>
//   }
// }

// module GroupTable = {
//   @react.component
//   let make = (~store: Store.t, ~group: UserGroupTypes.get_response) => {
//     <table className="pf-c-table pf-m-compact pf-m-grid-md" role="grid">
//       <RowItem.Head />
//       <tbody role="rowgroup">
//         {group.users
//         ->Belt.List.mapWithIndex((idx, user) => <RowItem key={string_of_int(idx)} store user />)
//         ->Belt.List.toArray
//         ->React.array}
//       </tbody>
//     </table>
//   }
// }

// @react.component
// let make = (~group: string, ~store: Store.t) => {
//   let (state, _) = store
//   let groupName = group->Js.Global.decodeURIComponent
//   let request: ConfigTypes.get_group_members_request = {
//     index: state.index,
//     group: groupName,
//   }
//   let title = groupName
//   let tooltip_content = "This shows the list of group members"
//   let icon = <Patternfly.Icons.UsersAlt />
//   <MonoCard title tooltip_content icon>
//     <p> {"TODO"->str} </p>
//     // <NetworkRender
//     //   get={() => WebApi.UserGroup.get(request)}
//     //   trigger={state.query}
//     //   render={group => <GroupTable store group />}
//     // />
//   </MonoCard>
// }
// let default = make

module UserItem = {
  @react.component
  let make = (~store: Store.t, ~name: string) => {
    let (state, dispatch) = store
    let link = "/" ++ state.index ++ "/author/" ++ name->Js.Global.encodeURIComponent
    <MSimpleCard
      style={ReactDOM.Style.make(~cursor="pointer", ())}
      onClick={e => {
        e->ReactEvent.Mouse.preventDefault
        ChangeActivity->SetAuthorScopedTab->dispatch
        link->RescriptReactRouter.push
      }}>
      {name->str}
    </MSimpleCard>
  }
}

@react.component
let make = (~group: string, ~store: Store.t) => {
  let (state, _) = store
  let title = "User Group members"
  let tooltip_content = "This shows the list of group's members"
  let icon = <Patternfly.Icons.Users />
  let trigger = ""

  let toItems = (resp: ConfigTypes.get_group_members_response) => {
    resp.members->Belt.List.map(name => <UserItem store key={name} name />)
  }

  <MonoCard title tooltip_content icon>
    <NetworkRender
      get={() =>
        WebApi.Config.getGroupMembers({ConfigTypes.index: state.index, ConfigTypes.group: group})}
      trigger
      render={resp => resp->toItems->Belt.List.toArray->React.array}
    />
  </MonoCard>
}

let default = make
