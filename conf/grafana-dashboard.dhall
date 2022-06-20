let Grafana =
        env:DHALL_GRAFANA
      ? https://raw.githubusercontent.com/weeezes/dhall-grafana/f78d2887939dcb555a47a4b85a91a3d6b38ec2ea/package.dhall
          sha256:a0e1b5432090944fa671efce0085c6049019ae0d00ca289c268b4528d1cd39af

let datasource = Some (env:GRAFANA_DATASOURCE as Text ? "Prometheus")

let target =
      \(refId : Text) ->
      \(expr : Text) ->
      \(title : Text) ->
        Grafana.MetricsTargets.PrometheusTarget
          Grafana.PrometheusTarget::{
          , refId
          , expr
          , legendFormat = Some "{{ job }} : ${title}"
          }

let panel =
      \(y : Natural) ->
      \(title : Text) ->
      \(targets : List Grafana.MetricsTargets) ->
        Grafana.Panels.mkGraphPanel
          Grafana.GraphPanel::{
          , title
          , gridPos = { x = 0, y, w = 24, h = 6 }
          , datasource
          , targets
          , fill = 0
          , linewidth = 2
          }

let panels =
      [ panel
          0
          "Activity"
          [ target
              "A"
              "rate(http_request_duration_seconds_sum[5m]) / rate(http_request_duration_seconds_count[5m])"
              "Request latencies {{ method }} in seconds"
          ]
      , panel
          8
          "Memory"
          [ target
              "A"
              "increase(ghc_gcdetails_live_bytes[5m])"
              "Total amount of live data in the heap."
          ]
      , panel
          16
          "CPU"
          [ target
              "B"
              "increase(ghc_cpu_seconds_total[5m])"
              "CPU time in seconds"
          ]
      , panel
          24
          "API incoming requests count"
          [ target
              "C"
              "increase(query_check{job=\"api\"}[5m])"
              "API calls count on query_check endpoint"
          , target
              "D"
              "increase(query{job=\"api\"}[5m])"
              "API calls count on query endpoint"
          ]
      , panel
          32
          "API Authentication requests count"
          [ target
              "C"
              "increase(auth_success{job=\"api\"}[5m])"
              "Authentication redirections to Identity provider"
          , target
              "D"
              "increase(auth_provider_redirect{job=\"api\"}[5m])"
              "Authentications succeeded"
          ]
      , panel
          40
          "Crawler requests count (by crawler name)"
          [ target
              "E"
              "increase(http_request{job=\"crawler\", type=\"crawler\"}[5m])"
              "{{ ident }} requests"
          , target
              "F"
              "increase(http_failure{job=\"crawler\", type=\"crawler\"}[5m])"
              "{{ ident }} failures"
          ]
      , panel
          48
          "Crawler requests count (by url)"
          [ target
              "G"
              "sum by (url) (increase(http_request{job=\"crawler\", type=\"crawler\"}[5m]))"
              "{{ url }} requests"
          , target
              "H"
              "sum by (url) (increase(http_failure{job=\"crawler\", type=\"crawler\"}[5m]))"
              "{{ url }} failures"
          ]
      , panel
          56
          "Crawler requests to Monocle API"
          [ target
              "I"
              "increase(http_request{ident=\"api-client\", type=\"internal\"}[5m])"
              "requests"
          ]
      ]

in  Grafana.Dashboard::{
    , title = "Monocle"
    , editable = True
    , panels = Grafana.Utils.generateIds panels
    , links =
      [ Grafana.Link.Type.Link
          Grafana.LinkExternal::{
          , title = "Monocle"
          , url = "https://www.changemetrics.io"
          , tooltip = "change-metrics/monocle"
          }
      ]
    }
