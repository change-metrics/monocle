import React from 'react'

import Navbar from 'react-bootstrap/Navbar'

class TopMenu extends React.Component {
  render () {
    return <Navbar bg="light" expand="lg">
      <Navbar.Brand href={'/' + this.props.index}>Monocle</Navbar.Brand>
      <Navbar.Toggle aria-controls="basic-navbar-nav" />
    </Navbar>
  }
}

export default TopMenu
