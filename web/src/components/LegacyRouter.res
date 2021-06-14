type t = {push: string => unit}

module Link = {
  @react.component @module("react-router-dom")
  external make: (~_to: string, ~onClick: unit => unit, ~children: 'a) => React.element = "Link"
}
