@react.component
let make = (~name) =>
  <a href="" onClick={_ => RescriptReactRouter.push(name)}> {name->React.string} </a>

let default = make
