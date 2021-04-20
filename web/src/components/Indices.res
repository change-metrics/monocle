// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// Render indices
//
open Prelude

module Indice = {
  @react.component
  let make = (~name) =>
    <Tooltip2 position=#Bottom content={"Click to get the metric"}>
      <a href="" onClick={_ => RescriptReactRouter.push(name)}> {name->React.string} </a>
    </Tooltip2>

  let card: string => React.element = name => <MSimpleCard> {make({"name": name})} </MSimpleCard>
}

module Indices = {
  let cards = indices => indices->Belt.Array.map(Indice.card)->React.array

  @react.component
  let make = () => {
    let indices = useAutoGet(getIndices)
    <>
      <h2> {"Available Indices"->React.string} </h2>
      <Layout.Stack>
        {switch indices {
        | None => <Spinner />
        | Some(Error(title)) => <Alert variant=#Danger title />
        | Some(Ok(indices)) if indices->Array.length > 0 => cards(indices)
        | _ => <Alert variant=#Warning title={"Please create an index."} />
        }}
      </Layout.Stack>
    </>
  }
}

let default = Indices.make
