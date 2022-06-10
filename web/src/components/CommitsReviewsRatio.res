open Prelude

@react.component
let make = (~store: Store.t, ~extraQuery: option<string>=?) => {
  let title = "Commits VS Reviews Ratio"
  let tooltip_content =
    "This displays a ratio that show the amount of code review activities compared to the commit activities. " ++ "When it tends to the left it means more commits than code review is performed."

  let icon = <Patternfly.Icons.ResourcesAlmostFull />
  let (state, _) = store
  let baseRequest = Store.mkSearchRequest(state, SearchTypes.Query_ratio_commits_vs_reviews)
  let request = {
    ...baseRequest,
    query: switch extraQuery {
    | Some(ex) => addQuery(baseRequest.query, ex)
    | None => baseRequest.query
    },
  }
  let tokenM = state->Store.Store.getAuthenticatedUserJWT
  let trigger = state.query ++ extraQuery->Belt.Option.getWithDefault("")
  let match = resp =>
    switch resp {
    | SearchTypes.Ratio(value) => Some(value)
    | _ => None
    }
  let childrenBuilder = ratio =>
    <Patternfly.Layout.Flex>
      <Patternfly.Layout.FlexItem> {"Commits"->str} </Patternfly.Layout.FlexItem>
      <Patternfly.Layout.FlexItem>
        <Canvas.Dom width=200 height=20 onDraw={Canvas.drawScale(ratio->Js.Math.unsafe_round)} />
      </Patternfly.Layout.FlexItem>
      <Patternfly.Layout.FlexItem> {"Reviews"->str} </Patternfly.Layout.FlexItem>
    </Patternfly.Layout.Flex>

  <QueryRenderCard title tooltip_content icon request tokenM trigger match childrenBuilder />
}

let default = make
