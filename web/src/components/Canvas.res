// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// A small canvas binding
//

type t

@send external getContext: (Dom.element, string) => t = "getContext"
@send external clearRect: (t, int, int, int, int) => unit = "clearRect"
@send external beginPath: t => unit = "beginPath"
@send external arc: (t, int, int, int, int, float) => unit = "arc"
@send external rect: (t, float, float, float, float) => unit = "rect"
@send external fill: t => unit = "fill"
@send external moveTo: (t, int, int) => unit = "moveTo"
@send external fillRect: (t, int, int, int, int) => unit = "fillRect"
@send external lineTo: (t, int, int) => unit = "lineTo"
@send external stroke: t => unit = "stroke"

@set external setFillStyle: (t, string) => unit = "fillStyle"
@set external setStrokeStyle: (t, string) => unit = "strokeStyle"
@set external setLineWidth: (t, float) => unit = "lineWidth"

let drawScale = (pos, width, height, ctx) => {
  let padding = 5
  ctx->clearRect(0, 0, width, height)
  // ctx->setFillStyle("lightgrey")
  // ctx->fillRect(0, 0, width, height)

  // the scale
  ctx->setFillStyle("black")
  ctx->setLineWidth(0.5)
  ctx->beginPath

  // axis
  ctx->moveTo(padding, height / 2)
  ctx->lineTo(width - padding, height / 2)

  // ticks
  let drawTick = (pos, offset) => {
    ctx->moveTo(pos, padding + offset)
    ctx->lineTo(pos, height - padding - offset)
  }
  padding->drawTick(0)
  (width / 2)->drawTick(1)
  (width - padding)->drawTick(0)
  ctx->stroke

  // the pos
  let axisWidth = width - 2 * padding
  let posX = Belt.Int.toFloat(padding) +. Belt.Int.toFloat(pos * axisWidth) /. 100.0
  let posPadding = 1.5

  ctx->setLineWidth(1.0)
  ctx->setStrokeStyle("#cb4b16")
  ctx->beginPath
  ctx->rect(
    posX -. posPadding,
    Belt.Int.toFloat(padding),
    posPadding *. 2.0,
    Belt.Int.toFloat(height - padding * 2),
  )
  ctx->stroke
}

module Dom = {
  @react.component
  let make = (~width: int, ~height: int, ~onDraw: (int, int, t) => unit) => {
    let el = React.useRef(Js.Nullable.null)
    let setRef = element => {
      el.current = element
      element
      ->Js.Nullable.toOption
      ->Belt.Option.flatMap(element => onDraw(width, height, element->getContext("2d"))->Some)
      ->ignore
    }
    let toPx = x => string_of_int(x) ++ "px"
    <canvas ref={ReactDOM.Ref.callbackDomRef(setRef)} width={width->toPx} height={height->toPx} />
  }
}
