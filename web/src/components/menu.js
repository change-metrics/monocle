import React from 'react'

import Navbar from 'react-bootstrap/Navbar'
import PropTypes from 'prop-types'

class TopMenu extends React.Component {
  render () {
    return <Navbar bg="light" expand="lg">
      <Navbar.Brand href={'/' + this.props.index}>Monocle</Navbar.Brand>
      <Navbar.Toggle aria-controls="basic-navbar-nav" />
    </Navbar>
  }
}

TopMenu.propTypes = {
  index: PropTypes.string.isRequired
}

export default TopMenu
