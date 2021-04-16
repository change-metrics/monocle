open PatternflyExtra

module Indice = {
  @react.component
  let make = (~name) =>
    <Tooltip position=#Bottom content={"Click to get the metric"}>
      <a href="" onClick={_ => RescriptReactRouter.push(name)}> {name->React.string} </a>
    </Tooltip>
}

let indiceItem = name => <Card> <CardBody> <Indice name /> </CardBody> </Card>

module Indices = {
  @react.component
  let make = (~names) => {
    <List> {names->Belt.Array.map(indiceItem)->React.array} </List>
  }
}

let default = Indices.make
