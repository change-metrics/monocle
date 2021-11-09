// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// The main component
//
open Prelude

module MonocleNav = {
  @react.component
  let make = (~active: string, ~store: Store.t) => {
    let (state, _) = store

    let navItem = (name, dest) => {
      let query = [
        switch state.query {
        | "" => list{}
        | q => list{"q=" ++ q}
        },
        switch dest {
        | "/changes" =>
          switch state.order {
          | None => list{}
          | Some(_) => list{"o=" ++ state.order->orderToQS}
          }
        | _ => list{}
        },
      ]->Belt.List.concatMany
      let navUrl =
        "/" ++
        state.index ++
        dest ++
        switch query {
        | list{} => ""
        | _ => "?" ++ query->concatSep("&")
        }
      <NavItem
        key={name}
        onClick={_ => navUrl->RescriptReactRouter.push}
        isActive={active == dest}
        preventDefault={true}
        _to={navUrl}>
        {name->str}
      </NavItem>
    }

    let navGroup = (title, xs) =>
      <NavGroup title key={title}>
        {xs->Belt.List.map(((a, b)) => navItem(a, b))->Belt.List.toArray->React.array}
      </NavGroup>

    <Nav>
      <NavList>
        {[
          navItem("Activity", "/"),
          navGroup("Changes", list{("Review Board", "/board"), ("Browse Changes", "/changes")}),
          navGroup("Projects", list{("Projects", "/projects"), ("Repositories", "/repos")}),
          navGroup(
            "People",
            list{
              ("Active authors", "/active_authors"),
              ("Peers strength", "/peers_strength"),
              ("New contributors", "/new_authors"),
              ("Groups", "/user_groups"),
            },
          ),
        ]->React.array}
      </NavList>
    </Nav>
  }
}

let logoPath = "/logo.png"

module About = {
  let buildLink = (url, name) =>
    <a href={url} target="_blank" rel="noopener noreferrer"> {name->str} </a>
  let defaultLink = buildLink("https://changemetrics.io", "Learn more about Monocle")
  let buildLinkItems = (links: list<Web.ConfigTypes.about_about_link>) =>
    links
    ->Belt.List.map(l =>
      <TextListItem key={l.url} component=#Dt> {buildLink(l.url, l.name)} </TextListItem>
    )
    ->Belt.List.toArray
    ->React.array
  let buildCategoryLinks = (links: list<Web.ConfigTypes.about_about_link>) =>
    links
    ->Belt.List.map(l => l.category)
    ->Belt.List.reduce(list{}, (acc, item) =>
      acc->Belt.List.has(item, (a, b) => a == b) ? acc : acc->Belt.List.add(item)
    )
    ->Belt.List.map(cat =>
      <TextContent key={cat}>
        <h4> {cat->str} </h4>
        <TextList component=#Dl>
          {links->Belt.List.keep(i => i.category == cat)->buildLinkItems}
        </TextList>
      </TextContent>
    )
    ->Belt.List.toArray
    ->React.array

  @react.component
  let make = (~store, ~isOpen: bool, ~onClose: unit => unit) =>
    switch Store.Fetch.about(store) {
    | None => <Spinner />
    | Some(Error(title)) => <Alert variant=#Danger title />
    | Some(Ok({about})) =>
      switch about {
      | Some(about) =>
        <AboutModal
          isOpen onClose productName="Monocle" brandImageAlt="Monocle" brandImageSrc=logoPath>
          <TextContent>
            <h4> {"About Monocle"->str} </h4>
            <TextList component=#Dl>
              <TextListItem component=#Dt> {"Monocle Version"->str} </TextListItem>
              <TextListItem component=#Dd> {about.version} </TextListItem>
              <TextListItem component=#Dt> {defaultLink} </TextListItem>
            </TextList>
          </TextContent>
          {about.links->buildCategoryLinks}
        </AboutModal>
      | None => React.null
      }
    }
}

@react.component
let make = () => {
  let url = RescriptReactRouter.useUrl()

  // The initial index
  let initIndex = switch url.path->Belt.List.head->Belt.Option.getWithDefault("") {
  | "help" => ""
  | x => x
  }

  // The current nav
  let active = switch url.path {
  | list{} => ""
  | list{_, ...xs} => "/" ++ Js.Array.joinWith("/", xs->Belt.List.toArray)
  }

  let store = Store.use(initIndex)
  let (state, dispatch) = store
  let (showAbout, setShowAbout) = React.useState(_ => false)

  let _topNav = <Nav variant=#Horizontal> {<> </>} </Nav>
  let headerTools =
    <PageHeaderTools>
      <About store isOpen=showAbout onClose={() => setShowAbout(_ => false)} />
      <PageHeaderToolsGroup>
        <PageHeaderToolsItem>
          <div onClick={_ => setShowAbout(_ => true)}> <Patternfly.Icons.InfoAlt /> </div>
        </PageHeaderToolsItem>
      </PageHeaderToolsGroup>
    </PageHeaderTools>
  let nav = <MonocleNav active store />
  let sidebar = state.index == "" ? React.null : <PageSidebar nav />
  let logo = <span onClick={_ => store->Store.changeIndex("")}> <img src={logoPath} /> </span>
  let header = <PageHeader showNavToggle={state.index == "" ? false : true} logo headerTools />
  // This sep prevent footer from hidding page content, not pretty but this works!
  let sep = {<> <br /> <br /> <br /> </>}

  <>
    <Page header sidebar isManagedSidebar={true}>
      {state.index == ""
        ? React.null
        : <PageSection variant=#Dark> <Search.Top store /> </PageSection>}
      <PageSection isFilled={true}>
        {switch url.path {
        | list{} => <Indices.Indices store />
        | list{"help", "search"} => <HelpSearch.View store />
        | list{_} => <Activity store />
        | list{_, "active_authors"} => <ActivePeopleView store />
        | list{_, "peers_strength"} => <PeersStrengthView store />
        | list{_, "new_authors"} => <NewContributorsView store />
        | list{_, "projects"} => <ProjectsView store />
        | list{_, "user_groups"} => <GroupsView store />
        | list{_, "user_groups", group} => <GroupView group store />
        | list{_, "repos"} => <ReposView store />
        | list{_, "changes"} => <NChangeView store />
        | list{_, "change", change} => <ChangeView change store />
        | list{_, "board"} => <Board store />
        | _ => <p> {"Not found"->str} </p>
        }}
        {sep}
      </PageSection>
    </Page>
    <ul className="pf-c-alert-group pf-m-toast">
      {state.toasts
      ->Belt.List.mapWithIndex((idx, toast) =>
        <li className="pf-c-alert-group__item">
          <Patternfly.Alert
            key={idx->string_of_int}
            title={toast}
            timeout={4000}
            onTimeout={_ => toast->RemoveToast->dispatch}
          />
        </li>
      )
      ->Belt.List.toArray
      ->React.array}
    </ul>
  </>
}

let default = make
