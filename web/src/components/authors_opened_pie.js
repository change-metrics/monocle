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

import { connect } from 'react-redux'

import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import Card from 'react-bootstrap/Card'
import { withRouter } from 'react-router-dom'

import {
  LoadingBox,
  ErrorBox,
  BaseQueryComponent,
  mapDispatchToProps,
  addMap
} from './common'

import Pie from './pie'

class AuthorsOpenedPie extends BaseQueryComponent {
  constructor (props) {
    super(props)
    this.state.name = 'authors_top_opened'
    this.state.graph_type = 'authors_top_opened'
  }

  render () {
    if (!this.props.authors_top_opened_loading) {
      if (this.props.authors_top_opened_error) {
        return <ErrorBox
          error={this.props.authors_top_opened_error}
        />
      }
      if (!this.props.authors_top_opened_result) {
        return <ErrorBox
          error={{ data: 'No data for AuthorsOpenedPie', status: 0 }}
        />
      }
      return (
        <Row>
          <Col>
            <Card>
              <Card.Header>
                <Card.Title>Opened Changes per author</Card.Title>
              </Card.Header>
              <Card.Body>
                <Row>
                  <Col>
                    <Pie
                      field="authors"
                      history={this.props.history}
                      data={this.props.authors_top_opened_result}
                    />
                  </Col>
                </Row>
              </Card.Body>
            </Card>
          </Col>
        </Row>
      )
    } else {
      return <LoadingBox />
    }
  }
}

const mapStateToProps = state => addMap({}, state.QueryReducer, 'authors_top_opened')

const CAuthorsOpenedPie = withRouter(connect(mapStateToProps, mapDispatchToProps)(AuthorsOpenedPie))

export default CAuthorsOpenedPie
