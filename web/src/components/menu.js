import React from 'react'

import Navbar from 'react-bootstrap/Navbar'
import Nav from 'react-bootstrap/Nav'
import { Link } from 'react-router-dom'
import PropTypes from 'prop-types'

class TopMenu extends React.Component {
  render () {
    const search = window.location.search
    return <Navbar bg="light" expand="lg">
      <Navbar.Brand>
        <Link className="navbar-brand" to={`/r/${this.props.index}`}>Monocle</Link>
      </Navbar.Brand>
      <Nav className="mr-auto">
        <Link className="nav-link" to={`/${this.props.index}${search}`}>Top</Link>
        <Link className="nav-link" to={`/${this.props.index}/merged-changes${search}`}>Merged</Link>
        <Link className="nav-link" to={`/${this.props.index}/opened-changes${search}`}>Opened</Link>
      </Nav>
      <Navbar.Toggle aria-controls="basic-navbar-nav" />
    </Navbar>
  }
}

TopMenu.propTypes = {
  index: PropTypes.string.isRequired
}

export default TopMenu
