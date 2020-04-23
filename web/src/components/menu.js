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

import Navbar from 'react-bootstrap/Navbar'
import Nav from 'react-bootstrap/Nav'
import { Link } from 'react-router-dom'
import PropTypes from 'prop-types'

import MyLink from './my_link'

class TopMenu extends React.Component {
  render () {
    const search = window.location.search
    return <Navbar bg="light" expand="lg">
      <Navbar.Brand>
        <Link className="navbar-brand" to={`/r/${this.props.index}`}>Monocle</Link>
      </Navbar.Brand>
      <Nav className="mr-auto">
        <MyLink className="nav-link" to={`/${this.props.index}${search}`}>Top</MyLink>
        <MyLink className="nav-link" to={`/${this.props.index}/opened-changes${search}`}>Opened</MyLink>
        <MyLink className="nav-link" to={`/${this.props.index}/merged-changes${search}`}>Merged</MyLink>
        <MyLink className="nav-link" to={`/${this.props.index}/abandoned-changes${search}`}>Abandoned</MyLink>
      </Nav>
      <Navbar.Toggle aria-controls="basic-navbar-nav" />
    </Navbar>
  }
}

TopMenu.propTypes = {
  index: PropTypes.string.isRequired
}

export default TopMenu
