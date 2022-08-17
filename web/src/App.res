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
              ("Search authors", "/search_author"),
              ("Active authors", "/active_authors"),
              ("Peers strength", "/peers_strength"),
              ("New contributors", "/new_authors"),
              ("Groups", "/user_groups"),
            },
          ),
          navGroup("Metrics", list{("Catalog", "/metrics")}),
        ]->React.array}
      </NavList>
    </Nav>
  }
}

let isAuthEnforced = (state: Store.Store.t) =>
  switch state.about.auth {
  | Auth_config({provider_name: ""}) => false
  | Auth_config(auth) => auth.force_login
  }

module Login = {
  module Validator = {
    @react.component
    let make = (~store: Store.t, ~usernameValidated: string, ~setShowLoginModal) => {
      let (_, dispatch) = store
      let get = {() => WebApi.Login.loginValidation({username: usernameValidated}, None)}
      usernameValidated->String.length == 0
        ? React.null
        : {
            switch useAutoGetOn(get, usernameValidated) {
            | NotAsked => React.null
            | Loading(_) => <Spinner />
            | Failure(_) => <Alert variant=#Danger title={"Unable to verify author identity :("} />
            | Loaded(resp) =>
              switch (resp: LoginTypes.login_validation_response) {
              | LoginTypes.Validation_result(LoginTypes.Unknown_ident) =>
                <Alert variant=#Warning title={"Author identity not found"} />
              | LoginTypes.Validation_result(LoginTypes.Known_ident) => {
                  usernameValidated->NonAuthenticatedLogin->dispatch
                  setShowLoginModal(_ => false)
                  React.null
                }
              }
            }
          }
    }
  }

  module NonAuthenticatedLoginModal = {
    @react.component
    let make = (~store: Store.t, ~setShowLoginModal) => {
      let loginTitle = "Login on Monocle"
      let loginSubtitle = "This is not an authenticated login - Set your username to an author identity to get personalized content."
      let (username, setUsername) = React.useState(_ => "")
      let (usernameValidated, setUsernameValidated) = React.useState(_ => "")
      let onChange = (value, _) => {
        setUsername(_ => value)
      }
      let onClick = e => {
        e->ReactEvent.Mouse.preventDefault
        setUsernameValidated(_ => username)
      }
      let close = e => {
        e->ReactEvent.Mouse.preventDefault
        setShowLoginModal(_ => false)
      }

      <LoginPage loginTitle loginSubtitle>
        <Form>
          <FormGroup label={"Username"} fieldId={"login"}>
            <TextInput id={"login"} onChange value={username} />
            <Validator store usernameValidated setShowLoginModal />
          </FormGroup>
          <ActionGroup>
            <Button _type=#Submit variant=#Primary onClick> {"Login"} </Button>
            <Button variant=#Primary onClick=close> {"Close"} </Button>
          </ActionGroup>
        </Form>
      </LoginPage>
    }
  }
  module AuthenticatedLoginModal = {
    @react.component
    let make = (~setShowLoginModal, ~closable, ~auth: Web.ConfigTypes.about_auth_config) => {
      let loginTitle = "Login on Monocle"
      let loginSubtitle =
        "Click on the button below to be redirected to identity provider." ++ " Once authenticated you'll be redirected to Monocle."
      let currentLocation = readWindowLocationFull()->Js.Global.encodeURIComponent
      let location = WebApi.serverUrl ++ "/" ++ "api/2/auth/login?redirectUri=" ++ currentLocation
      let onClick = e => {
        e->ReactEvent.Mouse.preventDefault
        replaceWindowLocation(location)
      }
      let close = e => {
        e->ReactEvent.Mouse.preventDefault
        setShowLoginModal(_ => false)
      }
      <LoginPage loginTitle loginSubtitle>
        <Form>
          <ActionGroup>
            <Button _type=#Submit variant=#Primary icon={<Patternfly.Icons.User />} onClick>
              {"Authenticate with " ++ auth.provider_name}
            </Button>
            {closable ? <Button variant=#Primary onClick=close> {"Close"} </Button> : React.null}
          </ActionGroup>
        </Form>
      </LoginPage>
    }
  }

  module LoginModal = {
    @react.component
    let make = (~store: Store.t, ~setShowLoginModal, ~closable=true) => {
      let (state, _) = store
      switch state.about.auth {
      | Auth_config({provider_name: ""}) => <NonAuthenticatedLoginModal store setShowLoginModal />
      | Auth_config(auth) => <AuthenticatedLoginModal setShowLoginModal closable auth />
      }
    }
  }

  module LoginStatus = {
    let displayLoggedStatus = (state: Store.Store.t, username, onClickLogout) => {
      <Patternfly.Layout.Flex>
        <Patternfly.Layout.FlexItem>
          {state.index != ""
            ? <Button
                variant=#Tertiary
                icon={<Patternfly.Icons.User color="cyan" title={username} />}
                onClick={_ => {
                  let homeUrl =
                    "/" ++
                    state.index ++
                    "/" ++
                    "author" ++
                    "/" ++
                    username->Js.Global.encodeURIComponent
                  homeUrl->RescriptReactRouter.push
                }}>
                {username}
              </Button>
            : <Button
                variant=#Tertiary
                isDisabled=true
                icon={<Patternfly.Icons.User color="cyan" title={username} />}>
                {username}
              </Button>}
        </Patternfly.Layout.FlexItem>
        <Patternfly.Layout.FlexItem>
          <div onClick={onClickLogout} style={ReactDOM.Style.make(~cursor="pointer", ())}>
            <Patternfly.Icons.Arrow color="coral" title="Logout" />
          </div>
        </Patternfly.Layout.FlexItem>
      </Patternfly.Layout.Flex>
    }

    @react.component
    let make = (~store: Store.t, ~setShowLoginModal) => {
      let (state, dispatch) = store
      let onClickLogin = _ => setShowLoginModal(_ => true)
      let onClickLogoutNAU = _ => NonAuthenticatedLogout->dispatch
      let onClickLogoutAU = _ => AuthenticatedLogout->dispatch
      let displayUID = (au: Jwt.authenticatedUser) =>
        switch state.index {
        | "" => au.defaultMuid
        | index =>
          switch Jwt.getMuidByIndex(au, index) {
          | Some(muid) => muid
          | None => au.defaultMuid
          }
        }
      <div style={ReactDOM.Style.make(~paddingRight="13px", ())}>
        {switch (state.username, state.authenticated_user) {
        | (Some(username), None) => displayLoggedStatus(state, username, onClickLogoutNAU)
        | (None, Some(authenticatedUser)) =>
          displayLoggedStatus(state, authenticatedUser->displayUID, onClickLogoutAU)
        | _ =>
          state->isAuthEnforced
            ? React.null
            : <Button variant=#Tertiary onClick=onClickLogin> {"Login"} </Button>
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
  let make = (~store: Store.t, ~isOpen: bool, ~onClose: unit => unit) => {
    let (state, _) = store
    <AboutModal isOpen onClose productName="Monocle" brandImageAlt="Monocle" brandImageSrc=logoPath>
      <TextContent>
        <h4> {"About Monocle"->str} </h4>
        <TextList component=#Dl>
          <TextListItem component=#Dt> {"Monocle Version"->str} </TextListItem>
          <TextListItem component=#Dd> {state.about.version} </TextListItem>
          <TextListItem component=#Dt> {defaultLink} </TextListItem>
        </TextList>
      </TextContent>
      {state.about.links->buildCategoryLinks}
    </AboutModal>
  }
}

module App = {
  @react.component
  let make = (~about: ConfigTypes.about) => {
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

    // Init the APP store
    let store = Store.use(initIndex, about)
    let (state, dispatch) = store

    // Some state for managing various modals
    let (showAbout, setShowAbout) = React.useState(_ => false)
    let (showLoginModal, setShowLoginModal) = React.useState(_ => false)

    // Remove the token 60s before its expiration
    React.useEffect0(() => {
      switch state.authenticated_user {
      | Some(au) => {
          let now = Js.Date.now()
          let willExpireAt = au.jwt_exp->Js.Date.getTime
          let willExpireIn = (willExpireAt -. now)->Belt.Int.fromFloat - 60
          Js.log3("JWT expiration in", willExpireIn / 1000, "s")
          Js.Global.setTimeout(() => AuthenticatedLogout->dispatch, willExpireIn)->ignore
        }
      | None => ()
      }
      None
    })

    // The main page to render APP
    let bodyPage = {
      let nav = <MonocleNav active store />
      let sidebar = state.index == "" ? React.null : <PageSidebar nav />
      let header = {
        let logo = <span onClick={_ => store->Store.changeIndex("")}> <img src={logoPath} /> </span>
        let headerTools =
          <PageHeaderTools>
            <About store isOpen=showAbout onClose={() => setShowAbout(_ => false)} />
            <PageHeaderToolsGroup>
              <PageHeaderToolsItem>
                <Login.LoginStatus store setShowLoginModal />
              </PageHeaderToolsItem>
              <PageHeaderToolsItem>
                <div
                  onClick={_ => setShowAbout(_ => true)}
                  style={ReactDOM.Style.make(~cursor="pointer", ())}>
                  <Patternfly.Icons.InfoAlt />
                </div>
              </PageHeaderToolsItem>
              // <PageHeaderToolsItem>
              //   <div
              //     onClick={showSettings}
              //     style={ReactDOM.Style.make(~cursor="pointer", ~paddingLeft="13px", ())}>
              //     <Patternfly.Icons.Cog />
              //   </div>
              // </PageHeaderToolsItem>
            </PageHeaderToolsGroup>
          </PageHeaderTools>
        <PageHeader showNavToggle={state.index == "" ? false : true} logo headerTools />
      }
      <Page header sidebar isManagedSidebar={true}>
        <React.Fragment>
          {state.index != ""
            ? <PageSection variant=#Dark> <Search.Top store /> </PageSection>
            : React.null}
          {switch (showLoginModal, state->isAuthEnforced, state.authenticated_user) {
          | (true, _, _) => <Login.LoginModal store setShowLoginModal />
          | (_, true, None) => <Login.LoginModal store setShowLoginModal closable=false />
          | _ =>
            <PageSection isFilled={true}>
              {switch url.path {
              | list{} => <Indices.Indices store />
              | list{"help", "search"} => <HelpSearch.View store />
              | list{_, "settings"} => <LocalSettings.View store />
              | list{_} => <Activity store />
              | list{_, "author", name} => <ScopedView.AuthorScopedView store name />
              | list{_, "group", name} => <ScopedView.GroupScopedView store name />
              | list{_, "active_authors"} => <ActivePeopleView store />
              | list{_, "peers_strength"} => <PeersStrengthView store stacked={false} />
              | list{_, "new_authors"} => <NewContributorsView store />
              | list{_, "projects"} => <ProjectsView store />
              | list{_, "user_groups"} => <GroupsView store />
              | list{_, "user_groups", group} => <GroupView group store />
              | list{_, "repos"} => <ReposView store />
              | list{_, "changes"} => <NChangeView store />
              | list{_, "change", change} => <ChangeView change store />
              | list{_, "board"} => <Board store />
              | list{_, "search_author"} => <AuthorSearch store />
              | list{_, "metrics"} => <Metrics store />
              | list{_, "metric", name} => <Metric store name />
              | _ => <p> {"Not found"->str} </p>
              }}
            </PageSection>
          }}
        </React.Fragment>
      </Page>
    }

    let toaster =
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

    <React.Fragment> bodyPage toaster </React.Fragment>
  }
  // let showSettings = _ => "settings"->RescriptReactRouter.push
}

@react.component
let make = () =>
  <NetworkRender
    get={() => WebApi.Config.getAbout({void: ""}, None)}
    trigger={""}
    render={(resp: ConfigTypes.get_about_response) => {
      switch resp.about {
      | Some(about) => <App about />
      | None => <Alert variant=#Danger title={"Unable to fetch about data !"} />
      }
    }}
  />

let default = make
