open Prelude

@react.component
let make = (~store: Store.t) => {
  let title = "Search authors"
  let tooltip_content = "Use this form to search authors over all event authors"
  let icon = <Patternfly.Icons.Users />
  let (state, _) = store
  let hxvals = Js.String2.concatMany("{\"index\": \"", [state.index, "\"}"])
  <Card isCompact=true>
    <CardTitle>
      <MGrid>
        <MGridItemXl9>
          <Title headingLevel=#H3>
            <Tooltip content=tooltip_content> {icon} </Tooltip> {(" " ++ title)->str}
          </Title>
        </MGridItemXl9>
      </MGrid>
    </CardTitle>
    <CardBody>
      <HTMXGetHook url="/api/2/htmx/authors_search" trigger="load" hxVals=hxvals>
        <p> {"loading..."->str} </p>
      </HTMXGetHook>
    </CardBody>
  </Card>
}

let default = make
