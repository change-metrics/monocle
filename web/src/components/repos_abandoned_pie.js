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

class ReposAbandonedPie extends BaseQueryComponent {
  constructor (props) {
    super(props)
    this.state.name = 'repos_top'
    this.state.graph_type = 'repos_top_abandoned'
    this.state.state = 'CLOSED'
  }

  render () {
    if (!this.props.repos_top_abandoned_loading) {
      if (this.props.repos_top_abandoned_error) {
        return <ErrorBox
          error={this.props.repos_top_abandoned_error}
        />
      }
      if (!this.props.repos_top_abandoned_result) {
        return <ErrorBox
          error={{ data: 'No data for ReposAbandonedPie', status: 0 }}
        />
      }
      return (
        <Row>
          <Col>
            <Card>
              <Card.Header>
                <Card.Title>Abandoned Changes per repository</Card.Title>
              </Card.Header>
              <Card.Body>
                <Row>
                  <Col>
                    <Pie
                      field="repository"
                      history={this.props.history}
                      data={this.props.repos_top_abandoned_result}
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

const mapStateToProps = state => addMap({}, state.QueryReducer, 'repos_top_abandoned')

const CReposAbandonedPie = withRouter(connect(mapStateToProps, mapDispatchToProps)(ReposAbandonedPie))

export default CReposAbandonedPie
