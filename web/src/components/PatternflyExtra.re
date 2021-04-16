// Missing binding from re-patternfly

include Patternfly;

module Tooltip = {
  [@react.component] [@module "@patternfly/react-core"]
  external make:
    (
      ~children: 'children,
      ~position: [@bs.string] [
                   | [@bs.as "top"] `Top
                   | [@bs.as "bottom"] `Bottom
                 ]
                   =?,
      ~content: string=?
    ) =>
    React.element =
    "Tooltip";
};
