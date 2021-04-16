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

import Container from 'react-bootstrap/Container'
import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'

import { connect } from 'react-redux'
import { query } from './reducers/indices'

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
      element = (
      <>
      <Row>
        <Col>
          <div className="container-fluid py-5 bg-white mb-4 mt-4">
            <h2 className="text-center pt-5 pb-5">Available Indices in Monocle:</h2>
          </div>
        <Container>
            <Row className="row row-cols-1 row-cols-sm-2 row-cols-md-3 g-3">
              {this.props.indices.map((elt, idx) => <Col className="col-md-6 p-5 px-2 bg-white rounded border" key={idx}><Link to={'/' + elt}><h3><div className="text-center">{elt}</div></h3></Link></Col>)}
            </Row>
          </Container>
        </Col>
      </Row>
        </>)
    }
    return element
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
