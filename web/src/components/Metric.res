// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later

open Prelude
include Patternfly

module MetricCompute = {
  @react.component
  let make = (~store: Store.t, ~metric: string) => {
    let (state, _dispatch) = store
    let title = metric ++ " compute"
    let tooltip_content = ""
    let icon = <Patternfly.Icons.Bundle />
    let tokenM = state->Store.Store.getAuthenticatedUserJWT
    let compute: MetricTypes.compute = MetricTypes.default_compute()
    let trigger = state.query
    <MonoCard title tooltip_content icon>
      <NetworkRender
        get={() =>
          WebApi.Metric.get(
            {
              index: state.index,
              username: state.username->Belt.Option.getWithDefault(""),
              query: state.query,
              metric: metric,
              options: Compute(compute),
            },
            tokenM,
          )}
        trigger
        render={(resp: MetricTypes.get_response) => {
          switch resp {
          | MetricTypes.Int_value(v) => <p> {v->int32_str->str} </p>
          | MetricTypes.Float_value(v) => <p> {v->Belt.Float.toString->str} </p>
          | _ => <p> {"Not supported"->str} </p>
          }
        }}
      />
    </MonoCard>
  }
}

module MetricTop = {
  @react.component
  let make = (~store: Store.t, ~metric: string) => {
    let (state, _dispatch) = store
    let title = metric ++ " top"
    let tooltip_content = ""
    let icon = <Patternfly.Icons.Bundle />
    let tokenM = state->Store.Store.getAuthenticatedUserJWT
    let columnNames = ["Name", "Count"]
    let link = ActivePeopleView.TopTermsTable.NoLink
    let limit_values = list{10, 25, 50, 100, 500}
    let (limit, setLimit) = React.useState(() => 10)
    let top: MetricTypes.top = {limit: limit->Int32.of_int}
    let trigger = state.query ++ limit->string_of_int
    <MonoCard title tooltip_content icon>
      <NetworkRender
        get={() =>
          WebApi.Metric.get(
            {
              index: state.index,
              username: state.username->Belt.Option.getWithDefault(""),
              query: state.query,
              metric: metric,
              options: MetricTypes.Top(top),
            },
            tokenM,
          )}
        trigger
        render={(resp: MetricTypes.get_response) => {
          <React.Fragment>
            <LimitSelector limit setLimit default=10 values=limit_values />
            {switch resp {
            | MetricTypes.Top_int(v) =>
              <ActivePeopleView.TopTermsTable store items=v.termcount columnNames link />
            | _ => <p> {"Not supported"->str} </p>
            }}
          </React.Fragment>
        }}
      />
    </MonoCard>
  }
}

module MetricTrend = {
  module SimpleHisto = {
    @react.component @module("./chartjs.jsx")
    external make: (~histo: array<MetricTypes.histo_float>, ~label: string) => React.element =
      "SimpleHisto"
  }
  type intervals = {
    auto: bool,
    hour: bool,
    day: bool,
    week: bool,
    month: bool,
    year: bool,
  }
  @react.component
  let make = (~store: Store.t, ~metric: string) => {
    let (state, _dispatch) = store
    let title = metric ++ " trend"
    let tooltip_content = ""
    let icon = <Patternfly.Icons.Bundle />
    let tokenM = state->Store.Store.getAuthenticatedUserJWT
    let defaultInterval = {
      auto: false,
      hour: false,
      day: false,
      week: false,
      month: false,
      year: false,
    }
    let (interval, setInterval) = React.useState(_ => {...defaultInterval, auto: true})
    let getInterval = () => {
      interval.hour
        ? "hour"
        : interval.day
        ? "day"
        : interval.week
        ? "week"
        : interval.month
        ? "month"
        : interval.year
        ? "year"
        : ""
    }
    let trigger = state.query ++ getInterval()
    let trend: MetricTypes.trend = {interval: getInterval()}
    let toHistoFloat = (histo_int: list<MetricTypes.histo_int>): list<MetricTypes.histo_float> =>
      histo_int->Belt.List.map(b => {
        let nb: MetricTypes.histo_float = {date: b.date, count: b.count->Int32.to_float}
        nb
      })
    <MonoCard title tooltip_content icon>
      <NetworkRender
        get={() =>
          WebApi.Metric.get(
            {
              index: state.index,
              username: state.username->Belt.Option.getWithDefault(""),
              query: state.query,
              metric: metric,
              options: MetricTypes.Trend(trend),
            },
            tokenM,
          )}
        trigger
        render={(resp: MetricTypes.get_response) => {
          <React.Fragment>
            <Form>
              <FormGroup fieldId="interval" isInline=true>
                <Radio
                  id="Auto"
                  name="Auto"
                  label="Auto"
                  isChecked=interval.auto
                  onChange={(_, _) => setInterval(_ => {...defaultInterval, auto: true})}
                />
                <Radio
                  id="Hour"
                  name="Hour"
                  label="Hour"
                  isChecked=interval.hour
                  onChange={(_, _) => setInterval(_ => {...defaultInterval, hour: true})}
                />
                <Radio
                  id="Day"
                  name="Day"
                  label="Day"
                  isChecked=interval.day
                  onChange={(_, _) => setInterval(_ => {...defaultInterval, day: true})}
                />
                <Radio
                  id="Week"
                  name="Week"
                  label="Week"
                  isChecked=interval.week
                  onChange={(_, _) => setInterval(_ => {...defaultInterval, week: true})}
                />
                <Radio
                  id="Month"
                  name="Month"
                  label="Month"
                  isChecked=interval.month
                  onChange={(_, _) => setInterval(_ => {...defaultInterval, month: true})}
                />
                <Radio
                  id="Year"
                  name="Year"
                  label="Year"
                  isChecked=interval.year
                  onChange={(_, _) => setInterval(_ => {...defaultInterval, year: true})}
                />
              </FormGroup>
              {switch resp {
              | MetricTypes.Histo_int(v) =>
                <SimpleHisto histo={v.histo->toHistoFloat->Belt.List.toArray} label=metric />
              | MetricTypes.Histo_float(v) =>
                <SimpleHisto histo={v.histo->Belt.List.toArray} label=metric />
              | _ => <p> {"Not supported"->str} </p>
              }}
            </Form>
          </React.Fragment>
        }}
      />
    </MonoCard>
  }
}

@react.component
let make = (~store: Store.t, ~name: string) => {
  <MCenteredContent>
    <MetricCompute store metric=name />
    <MetricTrend store metric=name />
    <MetricTop store metric=name />
  </MCenteredContent>
}

let default = make
