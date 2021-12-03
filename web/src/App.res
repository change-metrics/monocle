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

module Login = {
  module Validator = {
    let checkResponse = (resp, setIsDisabled) =>
      switch (resp: LoginTypes.login_validation_response) {
      | LoginTypes.Validation_result(LoginTypes.Unknown_ident) =>
        <Alert variant=#Warning title={"Unknown username"} />
      | LoginTypes.Validation_result(LoginTypes.Known_ident) => {
          setIsDisabled(_ => false)
          React.null
        }
      }

    @react.component
    let make = (~username: string, ~setIsDisabled) => {
      let get = {() => WebApi.Login.loginValidation({username: username})}
      switch useAutoGetOn(get, username) {
      | NotAsked => {
          setIsDisabled(_ => true)
          React.null
        }
      | Loading(_) => {
          setIsDisabled(_ => true)
          React.null
        }
      | Failure(_) => <Alert variant=#Danger title={"Unable to verify username :("} />
      | Loaded(resp) => resp->checkResponse(setIsDisabled)
      }
    }
  }
  module Modal = {
    @react.component
    let make = (~store: Store.t, ~setShowLoginModal) => {
      let loginTitle = "Set your username"
      let (_, dispatch) = store
      let (username, setUsername) = React.useState(_ => "")
      let (isDisabled, setIsDisabled) = React.useState(_ => false)
      let onChange = (value, _) => {
        setUsername(_ => value)
      }
      let onClick = _ => {
        username->Login->dispatch
        setShowLoginModal(_ => false)
      }

      <LoginPage loginTitle>
        <Form>
          <FormGroup label={"Username"} fieldId={"login"}>
            <TextInput id={"login"} onChange value={username} />
            <Validator username setIsDisabled />
          </FormGroup>
          <ActionGroup>
            <Button _type=#Submit variant=#Primary onClick isDisabled> {"Login"} </Button>
          </ActionGroup>
        </Form>
      </LoginPage>
    }
  }
  module Button = {
    @react.component
    let make = (~store: Store.t, ~setShowLoginModal) => {
      let (state, dispatch) = store
      let onClickLogin = _ => setShowLoginModal(_ => true)
      let onClickLogout = _ => Logout->dispatch
      <div style={ReactDOM.Style.make(~paddingRight="13px", ())}>
        {switch state.username {
        | Some(username) =>
          <Tooltip content={"Click to logout"}>
            <Button variant=#Tertiary icon={<Patternfly.Icons.User />} onClick={onClickLogout}>
              {username}
            </Button>
          </Tooltip>
        | None => <Button variant=#Tertiary onClick=onClickLogin> {"Login"} </Button>
        }}
      </div>
    }
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
    | None => React.null
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
  let (showLoginModal, setShowLoginModal) = React.useState(_ => false)

  // let showSettings = _ => "settings"->RescriptReactRouter.push
  let _topNav = <Nav variant=#Horizontal> {<> </>} </Nav>
  let headerTools =
    <PageHeaderTools>
      <About store isOpen=showAbout onClose={() => setShowAbout(_ => false)} />
      <PageHeaderToolsGroup>
        <PageHeaderToolsItem> <Login.Button store setShowLoginModal /> </PageHeaderToolsItem>
        <PageHeaderToolsItem>
          <div
            onClick={_ => setShowAbout(_ => true)}
            style={ReactDOM.Style.make(~cursor="pointer", ())}>
            <Patternfly.Icons.InfoAlt />
          </div>
        </PageHeaderToolsItem>
        /*
        <PageHeaderToolsItem>
          <div
            onClick={showSettings}
            style={ReactDOM.Style.make(~cursor="pointer", ~paddingLeft="13px", ())}>
            <Patternfly.Icons.Cog />
          </div>
        </PageHeaderToolsItem>
 */
      </PageHeaderToolsGroup>
    </PageHeaderTools>
  let nav = <MonocleNav active store />
  let sidebar = state.index == "" ? React.null : <PageSidebar nav />
  let logo = <span onClick={_ => store->Store.changeIndex("")}> <img src={logoPath} /> </span>
  let header = <PageHeader showNavToggle={state.index == "" ? false : true} logo headerTools />

  <>
    <Page header sidebar isManagedSidebar={true}>
      {switch showLoginModal {
      | true => <Login.Modal store setShowLoginModal />
      | false => <>
          {switch state.index {
          | "" => React.null
          | _ => <PageSection variant=#Dark> <Search.Top store /> </PageSection>
          }}
          <PageSection isFilled={true}>
            {switch url.path {
            | list{} => <Indices.Indices store />
            | list{"help", "search"} => <HelpSearch.View store />
            | list{_, "settings"} => <LocalSettings.View store />
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
          </PageSection>
        </>
      }}
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
