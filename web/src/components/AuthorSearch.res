open Prelude

module AuthorItem = {
  @react.component
  let make = (~store: Store.t, ~author: SearchTypes.author) => {
    let (state, _) = store
    let authorLink = "/" ++ state.index ++ "/author/" ++ author.muid->Js.Global.encodeURIComponent
    let groupLink = group => "/" ++ state.index ++ "/group/" ++ group->Js.Global.encodeURIComponent
    let toAliasesList = aliases =>
      <List variant=#Inline>
        {aliases
        ->Belt.List.map(alias => <ListItem key={alias}> {alias} </ListItem>)
        ->Belt.List.toArray
        ->React.array}
      </List>
    let toGroupList = groups =>
      <List variant=#Inline>
        {groups
        ->Belt.List.map(group =>
          <ListItem key={group}>
            <a onClick={_ => group->groupLink->RescriptReactRouter.push}> {group->str} </a>
          </ListItem>
        )
        ->Belt.List.toArray
        ->React.array}
      </List>

    let details = (author: SearchTypes.author) =>
      author.aliases->Belt.List.length > 0
        ? <>
            <Patternfly.Divider isVertical={true} />
            <Patternfly.Layout.FlexItem>
              {author.aliases->toAliasesList}
            </Patternfly.Layout.FlexItem>
            <Patternfly.Divider isVertical={true} />
            <Patternfly.Layout.FlexItem> {author.groups->toGroupList} </Patternfly.Layout.FlexItem>
          </>
        : React.null

    <MSimpleCard>
      <Patternfly.Layout.Flex>
        <Patternfly.Layout.FlexItem>
          <a onClick={_ => authorLink->RescriptReactRouter.push}> <b> {author.muid->str} </b> </a>
        </Patternfly.Layout.FlexItem>
        {author->details}
      </Patternfly.Layout.Flex>
    </MSimpleCard>
  }
}

@react.component
let make = (~store: Store.t) => {
  let (state, _) = store

  let (matchQuery, setMatchQuery) = React.useState(_ => "")
  let (currentTimeoutId, setCurrentTimeoutId) = React.useState(_ => None)
  let (trigger, setTrigger) = React.useState(_ => "")

  let title = "Search an author"
  let tooltip_content = "Use this form to search authors over all event authors"
  let icon = <Patternfly.Icons.Users />

  let onChange = (input, _) => {
    setMatchQuery(_ => input)
    switch currentTimeoutId {
    | Some(tid) => Js.Global.clearTimeout(tid)
    | None => ()
    }
    matchQuery->Js.String2.length > 1
      ? setCurrentTimeoutId(_ =>
          Js.Global.setTimeout(() => setTrigger(_ => matchQuery), 1500)->Some
        )
      : ()
  }

  let toAuthors = (resp: SearchTypes.author_response) => {
    resp.authors->Belt.List.map(author => <AuthorItem key={author.muid} store author />)
  }

  <MonoCard title tooltip_content icon>
    <Patternfly.Layout.Stack hasGutter={true}>
      <Patternfly.Layout.StackItem>
        <Patternfly.TextInput id="matchQuery" _type=#Text value={matchQuery} onChange />
      </Patternfly.Layout.StackItem>
      <Patternfly.Layout.StackItem>
        <NetworkRender
          get={() =>
            WebApi.Search.author({
              SearchTypes.index: state.index,
              SearchTypes.query: matchQuery,
            })}
          trigger
          render={resp => resp->toAuthors->Belt.List.toArray->React.array}
        />
      </Patternfly.Layout.StackItem>
    </Patternfly.Layout.Stack>
  </MonoCard>
}

let default = make
