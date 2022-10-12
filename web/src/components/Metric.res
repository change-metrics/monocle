// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later

open Prelude
include Patternfly

module MetricInfo = {
  @react.component
  let make = (~metric: string) => {
    let title = "Metric Info"
    let tooltip_content = ""
    let icon = <Patternfly.Icons.InfoAlt />
    <MonoCard title tooltip_content icon>
      <NetworkRender
        get={() =>
          WebApi.Metric.info({
            metric: metric,
          })}
        trigger={""}
        render={(resp: MetricTypes.info_response) => {
          switch resp {
          | MetricTypes.Info(i) =>
            <React.Fragment>
              <p> <b> {"Metric Name: "->str} </b> {i.name->str} </p>
              <p> <b> {"Metric ID: "->str} </b> {i.metric->str} </p>
              <p> <b> {"Metric Description: "->str} </b> {i.long_description->str} </p>
            </React.Fragment>
          | _ => <p> {"Not supported"->str} </p>
          }
        }}
      />
    </MonoCard>
  }
}

module MetricCompute = {
  module Count = {
    @react.component
    let make = (~value: string) => {
      <TextContent> <Text component=#H1> {value->str} </Text> </TextContent>
    }
  }
  @react.component
  let make = (~store: Store.t, ~metric: string) => {
    let (state, _dispatch) = store
    let title = "Count"
    let tooltip_content = "The result of the metric computation"
    let icon = <Patternfly.Icons.Bundle />
    let compute: MetricTypes.compute = MetricTypes.default_compute()
    let trigger = state.query
    <MonoCard title tooltip_content icon>
      <NetworkRender
        get={() =>
          WebApi.Metric.get({
            index: state.index,
            username: state.username->Belt.Option.getWithDefault(""),
            query: state.query,
            metric: metric,
            options: Compute(compute),
          })}
        trigger
        render={(resp: MetricTypes.get_response) => {
          switch resp {
          | MetricTypes.Int_value(v) => <Count value={v->int32_str} />
          | MetricTypes.Float_value(v) => <Count value={v->Belt.Float.toString} />
          | MetricTypes.Duration_value({value: v}) =>
            <Count value={v->int32_str->momentHumanizeDuration} />
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
    let title = "Top"
    let tooltip_content = "This show the top terms for metrics based on a term aggregation."
    let icon = <Patternfly.Icons.Bars />
    let columnNames = ["Name", "Count"]
    let link = ActivePeopleView.TopTermsTable.NoLink
    let limit_values = list{10, 25, 50, 100, 500}
    let (limit, setLimit) = React.useState(() => 10)
    let top: MetricTypes.top = {limit: limit->Int32.of_int}
    let trigger = state.query ++ limit->string_of_int
    <MonoCard title tooltip_content icon>
      <NetworkRender
        get={() =>
          WebApi.Metric.get({
            index: state.index,
            username: state.username->Belt.Option.getWithDefault(""),
            query: state.query,
            metric: metric,
            options: MetricTypes.Top(top),
          })}
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
    external make: (
      ~histo: array<MetricTypes.histo_float>,
      ~label: string,
      ~isDuration: bool,
    ) => React.element = "SimpleHisto"
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
    let title = "Trend"
    let tooltip_content = "This shows the metric value trend over a customisable period"
    let icon = <Patternfly.Icons.TrendUp />
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
    let toHistoFloat' = (histo_int: list<MetricTypes.histo_duration>): list<
      MetricTypes.histo_float,
    > =>
      histo_int->Belt.List.map(b => {
        let nb: MetricTypes.histo_float = {date: b.date, count: b.count->Int32.to_float}
        nb
      })
    <MonoCard title tooltip_content icon>
      <NetworkRender
        get={() =>
          WebApi.Metric.get({
            index: state.index,
            username: state.username->Belt.Option.getWithDefault(""),
            query: state.query,
            metric: metric,
            options: MetricTypes.Trend(trend),
          })}
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
                <SimpleHisto
                  histo={v.histo->toHistoFloat->Belt.List.toArray} label=metric isDuration=false
                />
              | MetricTypes.Histo_float(v) =>
                <SimpleHisto histo={v.histo->Belt.List.toArray} label=metric isDuration=false />
              | MetricTypes.Histo_duration(v) =>
                <SimpleHisto
                  histo={v.histo->toHistoFloat'->Belt.List.toArray} label=metric isDuration=true
                />
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
    <MStack>
      <MStackItem>
        <MGrid>
          <MGridItemXl6> <MetricInfo metric=name /> </MGridItemXl6>
          <MGridItemXl6> <MetricCompute store metric=name /> </MGridItemXl6>
        </MGrid>
      </MStackItem>
      <MStackItem>
        <MGrid>
          <MGridItemXl6> <MetricTrend store metric=name /> </MGridItemXl6>
          <MGridItemXl6> <MetricTop store metric=name /> </MGridItemXl6>
        </MGrid>
      </MStackItem>
    </MStack>
  </MCenteredContent>
}

let default = make
