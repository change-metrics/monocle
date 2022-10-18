// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later

open Prelude
include Patternfly

module MetricList = {
  @react.component
  let make = (~store: Store.t, ~items: list<MetricTypes.metric_info>) => {
    let defaultSortedColumn = 0
    let columnNames = ["Name", "Description"]
    let isOrdered = (_, _, _) => false
    let formatters: list<MetricTypes.metric_info => React.element> = list{
      item => <MLink.MonoLink store filter={""} path={"metric/" ++ item.metric} name={item.name} />,
      item => item.description->str,
    }
    <SortableTable items defaultSortedColumn columnNames isOrdered formatters />
  }
}

@react.component
let make = (~store: Store.t) => {
  let title = "Metrics"
  let tooltip_content = "This shows the list of available metrics"
  let icon = <Patternfly.Icons.Bundle />
  <MCenteredContent>
    <MonoCard title tooltip_content icon>
      <NetworkRender
        get={() => WebApi.Metric.list({void: ""})}
        trigger={""}
        render={(resp: MetricTypes.list_response) => <MetricList store items=resp.metrics />}
      />
    </MonoCard>
  </MCenteredContent>
}

let default = make
