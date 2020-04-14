import React from 'react'

import Navbar from 'react-bootstrap/Navbar'
import { Link } from 'react-router-dom'
import PropTypes from 'prop-types'

class TopMenu extends React.Component {
  render () {
    return <Navbar bg="light" expand="lg">
      <Navbar.Brand ><Link className="navbar-brand" to={`/r/${this.props.index}`}>Monocle</Link></Navbar.Brand>
      <Navbar.Toggle aria-controls="basic-navbar-nav" />
    </Navbar>
  }
}

TopMenu.propTypes = {
  index: PropTypes.string.isRequired
}

export default TopMenu
