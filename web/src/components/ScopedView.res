open Prelude
open Activity
open ActivePeopleView

let buildView = (store: Store.t, entityTypeAsText: string, entityName: string, isGroup: bool) => {
  let (state, dispatch) = store
  let hideAuthors = isGroup ? false : true
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
          <MStackItem> <p> {" "->str} </p> </MStackItem>
          {isGroup
            ? {
                let byChangeCreated = {
                  let (qtype, _, tooltip_content, link) = ByChangeCreated->TopMetricsInfo.getQD
                  let title = "Top authors by change created"
                  <MostActiveAuthor store qtype title tooltip_content link extraQuery />
                }
                let byChangeMerged = {
                  let (qtype, _, tooltip_content, link) = ByChangeMerged->TopMetricsInfo.getQD
                  let title = "Top authors by change merged"
                  <MostActiveAuthor store qtype title tooltip_content link extraQuery />
                }
                <Layout.Grid sm=Column._12 xl=Column._6 hasGutter={true}>
                  <Layout.GridItem>
                    <ChangesLifeCycleStats store extraQuery hideAuthors />
                  </Layout.GridItem>
                  <Layout.GridItem>
                    <MStack>
                      <MStackItem> {byChangeCreated} </MStackItem>
                      <MStackItem> {byChangeMerged} </MStackItem>
                    </MStack>
                  </Layout.GridItem>
                </Layout.Grid>
              }
            : <MStackItem> <ChangesLifeCycleStats store extraQuery hideAuthors /> </MStackItem>}
        </MStack>
      </Tab>
      <Tab eventKey="2" title={<TabTitleText> "Review activity" </TabTitleText>}>
        <MStack>
          <MStackItem> <p> {" "->str} </p> </MStackItem>
          <MStackItem>
            <Layout.Grid sm=Column._12 xl=Column._6 hasGutter={true}>
              <Layout.GridItem>
                <ChangesReviewStats store extraQuery hideAuthors />
              </Layout.GridItem>
              <Layout.GridItem>
                <MStack>
                  <MStackItem>
                    {
                      let (qtype, _, tooltip_content, link) = ByMostReviewed->TopMetricsInfo.getQD
                      let title = "Most reviewed authors"
                      <MostActiveAuthor store qtype title tooltip_content link extraQuery />
                    }
                  </MStackItem>
                  <MStackItem>
                    {
                      let (qtype, _, tooltip_content, link) = ByMostCommented->TopMetricsInfo.getQD
                      let title = "Most commented authors"
                      <MostActiveAuthor store qtype title tooltip_content link extraQuery />
                    }
                  </MStackItem>
                  {maybeRender(
                    isGroup,
                    {
                      let byChangeReviewed = {
                        let (qtype, _, tooltip_content, link) =
                          ByChangeReviewed->TopMetricsInfo.getQD
                        let title = "Top authors by reviews"
                        <MostActiveAuthor store qtype title tooltip_content link extraQuery />
                      }
                      let byChangeCommented = {
                        let (qtype, _, tooltip_content, link) =
                          ByChangeCommented->TopMetricsInfo.getQD
                        let title = "Top authors by comments"
                        <MostActiveAuthor store qtype title tooltip_content link extraQuery />
                      }
                      <React.Fragment>
                        <MStackItem> {byChangeReviewed} </MStackItem>
                        <MStackItem> {byChangeCommented} </MStackItem>
                      </React.Fragment>
                    },
                  )}
                </MStack>
              </Layout.GridItem>
            </Layout.Grid>
          </MStackItem>
        </MStack>
      </Tab>
      <Tab eventKey="3" title={<TabTitleText> "Open changes" </TabTitleText>}>
        <MStack>
          <MStackItem>
            <NChangeView store extraQuery={extraQuery ++ " state:open"} hideAuthors />
          </MStackItem>
        </MStack>
      </Tab>
      <Tab eventKey="4" title={<TabTitleText> "Merged changes" </TabTitleText>}>
        <MStack>
          <MStackItem>
            <NChangeView store extraQuery={extraQuery ++ " state:merged"} hideAuthors />
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
