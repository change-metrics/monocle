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

const TITLE = (window.TITLE && window.TITLE !== '__TITLE__') ? window.TITLE : (process.env.REACT_APP_TITLE || 'Monocle')

class IndexMenu extends React.Component {
  render () {
    const search = window.location.search
    document.title = this.props.match.params.index ? TITLE + '/' + this.props.match.params.index : TITLE
    return (
      <Navbar.Collapse id="basic-navbar-nav">
        <Nav className="mr-auto">
          <Link className="nav-link" to={'/' + this.props.match.params.index + search}>Main</Link>
          <Link className="nav-link" to={'/' + this.props.match.params.index + '/people' + search}>People</Link>
          <Link className="nav-link" to={'/' + this.props.match.params.index + '/repos' + search}>Repositories</Link>
          <NavDropdown title="Changes" id="changes-nav-dropdown">
            <NavDropdown.Item onClick={() => this.props.history.push('/' + this.props.match.params.index + '/changes' + search)}>Browse changes</NavDropdown.Item>
            <NavDropdown.Item onClick={() => this.props.history.push('/' + this.props.match.params.index + '/hot-changes' + search)}>Hot changes</NavDropdown.Item>
            <NavDropdown.Item onClick={() => this.props.history.push('/' + this.props.match.params.index + '/cold-changes' + search)}>Cold changes</NavDropdown.Item>
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

class TopMenu extends React.Component {
  render () {
    document.title = TITLE
    return (
      <React.Fragment>
        <Navbar bg="light" expand="lg" sticky="top" className="fixed-top">
          <Navbar.Brand>
            <Link className="navbar-brand" to="/">{TITLE}</Link>
          </Navbar.Brand>
          <Switch>
            <Route exact path='/' />
            <Route path='/:index' component={IndexMenu} />
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

export default TopMenu
