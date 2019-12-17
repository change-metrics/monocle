import React from 'react';

import Nav from 'react-bootstrap/Nav'
import Navbar from 'react-bootstrap/Navbar'


class TopMenu extends React.Component {
  render() {
    return <Navbar bg="light" expand="lg">
      <Navbar.Brand href="/">Monocle</Navbar.Brand>
      <Navbar.Toggle aria-controls="basic-navbar-nav" />
    </Navbar>
  }
}

export default TopMenu
