open Prelude

%%raw(`var RescriptReactRouter = require("@rescript/react/src/RescriptReactRouter.bs.js")`)

@react.component
let make = () => {
  let title = "Search an author"
  let tooltip_content = "Use this form to search authors over all event authors"
  let icon = <Patternfly.Icons.Users />
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
      <HTMXGetHook url="/api/2/htmx/authors_search" trigger="load">
        <p> {"loading..."->str} </p>
      </HTMXGetHook>
    </CardBody>
  </Card>
}

let default = make
