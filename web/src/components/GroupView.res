open Prelude

@react.component
let make = (~group: string, ~store: Store.t) => {
  <div className="container"> <h2> {group->str} </h2> </div>
}

let default = make
