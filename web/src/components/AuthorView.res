open Prelude
open Activity

// This page should be accessed using an attribute to specify the author or group name
// The backend query is specified by adding the group or author attribute.

@react.component
let make = (~store: Store.t) => {
  let (state, _) = store
  let (activeTab, setActiveTag) = React.useState(_ => "1")
  let entityToSettings = state.author_scoped->Belt.Option.flatMap(author =>
    switch author {
    | Author(name) => ("Author", name)->Some
    | Group(name) => ("Group", name)->Some
    }
  )
  switch entityToSettings {
  | None => {
      Js.log("Unexpected empty author_scoped value")
      React.null
    }
  | Some((entityTypeAsText, entityName)) =>
    <MonoCard
      title={entityTypeAsText ++ " details for " ++ entityName}
      tooltip_content="content"
      icon={<Patternfly.Icons.User />}>
      <Tabs activeKey={activeTab} isBox=true onSelect={(_, key) => setActiveTag(_ => key)}>
        <Tab eventKey="1" isHidden=false title={<TabTitleText> "Change activity" </TabTitleText>}>
          <MStack> <MStackItem> <ChangesLifeCycleStats store /> </MStackItem> </MStack>
        </Tab>
        <Tab eventKey="2" isHidden=false title={<TabTitleText> "Review activity" </TabTitleText>}>
          <MStack> <MStackItem> <ChangesReviewStats store /> </MStackItem> </MStack>
        </Tab>
        <Tab eventKey="3" title={<TabTitleText> "Open changes" </TabTitleText>}>
          <MStack>
            <MStackItem> <NChangeView store contextQuery={"state:open"} /> </MStackItem>
          </MStack>
        </Tab>
        <Tab eventKey="4" title={<TabTitleText> "Merged changes" </TabTitleText>}>
          <MStack>
            <MStackItem> <NChangeView store contextQuery={"state:merged"} /> </MStackItem>
          </MStack>
        </Tab>
      </Tabs>
    </MonoCard>
  }
}

let default = make
