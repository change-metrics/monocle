// Copyright (C) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-or-later
//
// The monocle auth helpers
//

open Prelude

type t = {userid: string}

@module("../api.js") external whoAmIRaw: (t => unit, unit => unit) => unit = "getAuth"
@module("../api.js") external logout: unit => unit = "logout"

module Welcome = {
  @react.component
  let make = (~store: Store.t) => {
    // TODO: restore the current index from local storage
    RescriptReactRouter.push("/")
    <> <h3> {"Welcome!"->str} </h3> <Indices.Indices store /> </>
  }
}

module LoginPage = {
  @react.component
  let make = () =>
    <div className="container">
      <form key="login" method="post" action="/auth/github" target="_top">
        <input type_="hidden" name="back" value="/" />
        <Button isBlock=true _type=#Submit>
          <Icons.Github /> {"Log in with Github"->React.string}
        </Button>
      </form>
    </div>
}

module Button = {
  module Logout = {
    @react.component
    let make = (~onLogout, ~user) => <>
      <PageHeaderToolsItem> {("Welcome " ++ user)->str} </PageHeaderToolsItem>
      <PageHeaderToolsItem>
        <Button onClick={_ => onLogout()}> {"Logout"->str} </Button>
      </PageHeaderToolsItem>
    </>
  }

  module Login = {
    @react.component
    let make = (~onLogin) =>
      <PageHeaderToolsItem>
        <LegacyRouter.Link onClick=onLogin _to="/login">
          <Button> {"Login"->str} </Button>
        </LegacyRouter.Link>
      </PageHeaderToolsItem>
  }

  @react.component
  let make = (~store: Store.t) => {
    let (state, dispatch) = store
    let set = v => v->Store.Store.SetSession->dispatch
    let onLogout = () => {
      logout()
      Store.Session.Anonymous->set
    }
    let authOk = resp => {
      resp.userid->Store.Session.Authenticated->set
    }
    let onLogin = () => {
      let needAuth = () => {
        // Unfortunately, this does not refresh the react-router switch
        RescriptReactRouter.push("/login")
      }
      whoAmIRaw(authOk, needAuth)
    }
    React.useEffect0(_ => {
      let needAuth = () => Store.Session.Anonymous->set
      whoAmIRaw(authOk, needAuth)
      None
    })
    <PageHeaderToolsGroup>
      {switch state.session {
      | Store.Session.Unknown => "Loading..."->str
      | Store.Session.Anonymous => <Login onLogin />
      | Store.Session.Authenticated(user) => <Logout onLogout user />
      }}
    </PageHeaderToolsGroup>
  }
}
