open Prelude

module HistoBox = {
  @react.component
  let make = (~bucket: MetricTypes.histo) => {
    let count = bucket.count->Int32.to_int->Belt.Int.toFloat
    let countStr = switch bucket.count->Int32.to_int {
    | 0 => ""
    | x => x->string_of_int
    }
    let alpha = Js.Math.unsafe_round(255.0 *. Js.Math.log10(1.0 +. count))
    let green = 100 + Js.Math.unsafe_round(155.0 *. Js.Math.log10(1.0 +. count))
    let style = ReactDOM.Style.make(
      ~width="20px",
      ~height="20px",
      ~display="inline-block",
      ~border="1px solid black",
      ~margin="2px",
      ~borderRadius="5px",
      ~overflow="hidden",
      ~textAlign="center",
      ~backgroundColor="rgba(0, " ++ string_of_int(green) ++ ", 0, " ++ string_of_int(alpha) ++ ")",
      (),
    )
    <Tooltip content={bucket.date->str}> <span style> {countStr->str} </span> </Tooltip>
  }
}

module BaseHistoBox = {
  @react.component
  let make = (
    ~store: Store.t,
    ~title: string,
    ~tooltip_content: string,
    ~searchType: Web.SearchTypes.query_request_query_type,
    ~extraQuery: option<string>=?,
  ) => {
    let icon = <Patternfly.Icons.GripHorizontal />
    let (state, _) = store
    let baseRequest = Store.mkSearchRequest(state, searchType)
    let request = {
      ...baseRequest,
      query: switch extraQuery {
      | Some(ex) => addQuery(baseRequest.query, ex)
      | None => baseRequest.query
      },
    }
    let trigger = state.query ++ extraQuery->Belt.Option.getWithDefault("")
    let match = resp =>
      switch resp {
      | SearchTypes.Histo(data) => Some(data.histo)
      | _ => None
      }
    let childrenBuilder = data =>
      data
      ->Belt.List.mapWithIndex((index, bucket) => <HistoBox bucket key={index->string_of_int} />)
      ->Belt.List.toArray
      ->React.array

    let tokenM = state->Store.Store.getAuthenticatedUserJWT

    <QueryRenderCard tokenM title tooltip_content icon request trigger match childrenBuilder />
  }
}

module CommitsHistoBox = {
  @react.component
  let make = (~store: Store.t, ~extraQuery: option<string>=?) => {
    let title = "Commits activities"
    let tooltip_content = "This shows the commits activities"
    let searchType = SearchTypes.Query_histo_commits
    <BaseHistoBox store title tooltip_content searchType ?extraQuery />
  }
}

module ReviewsCommentsHistoBox = {
  @react.component
  let make = (~store: Store.t, ~extraQuery: option<string>=?) => {
    let title = "Reviews activities"
    let tooltip_content = "This shows the review activities"
    let searchType = SearchTypes.Query_histo_reviews_and_comments
    <BaseHistoBox store title tooltip_content searchType ?extraQuery />
  }
}
