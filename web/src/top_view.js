// Monocle.
// Copyright (C) 2020 Monocle authors

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
import { Link } from 'react-router-dom'
import PropTypes from 'prop-types'

import ListGroup from 'react-bootstrap/ListGroup'

import Container from 'react-bootstrap/Container'
import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'

import { connect } from 'react-redux'
import { query } from './reducers/indices'

import TopMenu from './components/menu'

import {
  ErrorBox,
  LoadingBox
} from './components/common'

class TopView extends React.Component {
  componentDidMount () {
    this.props.handleQuery()
  }

  render () {
    let element

    if (!this.props.indices) {
      element = <LoadingBox />
    } else if (this.props.indices.length === 0) {
      element = <ErrorBox error={{ status: 0, data: 'Please create an index.' }}/>
    } else {
      element = <Container>
        <h2>Available Indices</h2>
        <Row><Col><p></p></Col></Row>
        <Row>
          <Col>
            <ListGroup>
              {this.props.indices.map((elt, idx) => <ListGroup.Item key={idx}><Link to={`/${elt}`}>{elt}</Link></ListGroup.Item>)}
            </ListGroup>
          </Col>
        </Row>
      </Container>
    }
    return (
      <React.Fragment>
        <TopMenu />
        {element}
      </React.Fragment>
    )
  }
}

TopView.propTypes = {
  handleQuery: PropTypes.func,
  indices: PropTypes.array
}

const mapStateToProps = state => {
  return {
    indices: state.IndicesReducer.indices,
    error: state.IndicesReducer.indices_error
  }
}

const mapDispatchToProps = dispatch => {
  return {
    handleQuery: () => dispatch(query())
  }
}

export default connect(mapStateToProps, mapDispatchToProps)(TopView)
