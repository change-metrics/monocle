open Prelude
open Activity

let buildView = (store: Store.t, entityTypeAsText: string, entityName: string, isGroup: bool) => {
  let (state, dispatch) = store
  let displayAuthors = isGroup ? true : false
  let toSearchValue = (value: string) => "\"" ++ value ++ "\""
  let extraQuery = isGroup
    ? "group:" ++ toSearchValue(entityName)
    : "author:" ++ toSearchValue(entityName)
  <MonoCard
    title={entityTypeAsText ++ " details for " ++ entityName}
    tooltip_content={"This view is scoped to the " ++ entityName ++ " " ++ entityTypeAsText}
    icon={<Patternfly.Icons.User />}>
    <Tabs
      activeKey={state.author_scoped_tab}
      isBox=true
      onSelect={(_, key) => key->SetAuthorScopedTab->dispatch}>
      <Tab eventKey="1" title={<TabTitleText> "Change activity" </TabTitleText>}>
        <MStack>
          <MStackItem> <ChangesLifeCycleStats store extraQuery displayAuthors /> </MStackItem>
        </MStack>
      </Tab>
      <Tab eventKey="2" title={<TabTitleText> "Review activity" </TabTitleText>}>
        <MStack>
          <MStackItem> <ChangesReviewStats store extraQuery displayAuthors /> </MStackItem>
        </MStack>
      </Tab>
      <Tab eventKey="3" title={<TabTitleText> "Open changes" </TabTitleText>}>
        <MStack>
          <MStackItem> <NChangeView store extraQuery={extraQuery ++ " state:open"} /> </MStackItem>
        </MStack>
      </Tab>
      <Tab eventKey="4" title={<TabTitleText> "Merged changes" </TabTitleText>}>
        <MStack>
          <MStackItem>
            <NChangeView store extraQuery={extraQuery ++ " state:merged"} />
          </MStackItem>
        </MStack>
      </Tab>
      {isGroup
        ? <Tab eventKey="5" title={<TabTitleText> "Group members" </TabTitleText>}>
            <MStack> <MStackItem> <GroupView store group=entityName /> </MStackItem> </MStack>
          </Tab>
        : React.null}
    </Tabs>
  </MonoCard>
}

module AuthorScopedView = {
  @react.component
  let make = (~store: Store.t, ~name: string) =>
    buildView(store, "Author", name->Js.Global.decodeURIComponent, false)
}

module GroupScopedView = {
  @react.component
  let make = (~store: Store.t, ~name: string) =>
    buildView(store, "Group", name->Js.Global.decodeURIComponent, true)
}
