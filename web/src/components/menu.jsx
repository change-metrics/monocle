// Monocle.
// Copyright (C) 2019-2020 Monocle authors

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as published
// by the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.

// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

import React from 'react'
import { Switch, Route, Link } from 'react-router-dom'

import Navbar from 'react-bootstrap/Navbar'
import Nav from 'react-bootstrap/Nav'
import NavDropdown from 'react-bootstrap/NavDropdown'

import PropTypes from 'prop-types'
import { CUserView } from './user'
import { SmallSizeWarning } from './common'
import SearchTop from './SearchTop.bs.js'

const TITLE =
  window.TITLE !== '__TITLE__' ? window.TITLE : process.env.REACT_APP_TITLE

class IndexMenu extends React.Component {
  render() {
    const search = window.location.search
    document.title = this.props.match.params.index
      ? TITLE + '/' + this.props.match.params.index
      : TITLE
    return (
      <Navbar.Collapse id="basic-navbar-nav">
        <Nav className="mr-auto">
          <Link
            className="nav-link"
            to={'/' + this.props.match.params.index + search}
          >
            Activity
          </Link>
          <NavDropdown title="People" id="peropl-nav-dropdown">
            <NavDropdown.Item
              onClick={() =>
                this.props.history.push(
                  '/' + this.props.match.params.index + '/active_authors' + search
                )
              }
            >
              Active authors
            </NavDropdown.Item>
            <NavDropdown.Item
              onClick={() =>
                this.props.history.push(
                  '/' + this.props.match.params.index + '/peers_strength' + search
                )
              }
            >
              Peers strength
            </NavDropdown.Item>
            <NavDropdown.Item
              onClick={() =>
                this.props.history.push(
                  '/' + this.props.match.params.index + '/new_authors' + search
                )
              }
            >
              New authors
            </NavDropdown.Item>
            <NavDropdown.Item
              onClick={() =>
                this.props.history.push(
                  '/' + this.props.match.params.index + '/user_groups'
                )
              }
            >
              Groups
            </NavDropdown.Item>
          </NavDropdown>
          <NavDropdown title="Projects" id="changes-nav-dropdown">
            <NavDropdown.Item
              onClick={() =>
                this.props.history.push(
                  '/' + this.props.match.params.index + '/repos' + search
                )
              }
            >
              Repositories
            </NavDropdown.Item>
          </NavDropdown>
          <NavDropdown title="Changes" id="changes-nav-dropdown">
            <NavDropdown.Item
              onClick={() =>
                this.props.history.push(
                  '/' + this.props.match.params.index + '/changes' + search
                )
              }
            >
              Browse changes
            </NavDropdown.Item>
            <NavDropdown.Item
              onClick={() =>
                this.props.history.push(
                  '/' + this.props.match.params.index + '/board'
                )
              }
            >
              Board
            </NavDropdown.Item>
            <NavDropdown.Item
              onClick={() =>
                this.props.history.push(
                  '/' + this.props.match.params.index + '/hot-changes' + search
                )
              }
            >
              Hot changes
            </NavDropdown.Item>
            <NavDropdown.Item
              onClick={() =>
                this.props.history.push(
                  '/' + this.props.match.params.index + '/cold-changes' + search
                )
              }
            >
              Cold changes
            </NavDropdown.Item>
          </NavDropdown>
        </Nav>
      </Navbar.Collapse>
    )
  }
}

IndexMenu.propTypes = {
  history: PropTypes.shape({
    push: PropTypes.func
  }),
  match: PropTypes.shape({
    params: PropTypes.shape({
      index: PropTypes.string
    })
  })
}

const renderSearch = (store) => (prop) =>
  (
    <Nav className="ml-auto">
      <SearchTop store={store} />
    </Nav>
  )

class TopMenu extends React.Component {
  render() {
    document.title = TITLE
    const render = renderSearch(this.props.store)
    return (
      <React.Fragment>
        <Navbar bg="light" expand="lg" sticky="top" className="fixed-top">
          <Navbar.Brand>
            <Link className="navbar-brand" to="/">
              {TITLE}
            </Link>
          </Navbar.Brand>
          <Switch>
            <Route exact path="/" />
            <Route exact path="/help/search" />
            <Route path="/:index" component={IndexMenu} />
          </Switch>
          <Switch>
            <Route exact path="/:index/" render={render} />
            <Route path="/:index/board" render={render} />
            <Route path="/:index/user_groups" render={render} />
            <Route path="/:index/changes" render={render} />
            <Route path="/:index/change" render={render} />
            <Route path="/:index/repos" render={render} />
            <Route path="/:index/active_authors" render={render} />
            <Route path="/:index/peers_strength" render={render} />
            <Route path="/:index/new_authors" render={render} />
          </Switch>
          <Nav className="ml-auto">
            <CUserView />
          </Nav>
          <Navbar.Toggle aria-controls="basic-navbar-nav" />
        </Navbar>
        <SmallSizeWarning />
      </React.Fragment>
    )
  }
}

TopMenu.propTypes = {
  store: PropTypes.object
}

export default TopMenu
