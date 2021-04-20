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

class ReposPie extends BaseQueryComponent {
  constructor(props) {
    super(props)
    this.state.name = 'repos_top'
    this.state.graph_type = 'repos_top'
  }

  render() {
    if (!this.props.repos_top_loading) {
      if (this.props.repos_top_error) {
        return <ErrorBox error={this.props.repos_top_error} />
      }
      if (!this.props.repos_top_result) {
        return <ErrorBox error={{ data: 'No data for ReposPie', status: 0 }} />
      }
      return (
        <Card className="rounded border-0 border-top">
          <Card.Header className="bg-white text-center">
            <Card.Title>Changes per repository</Card.Title>
          </Card.Header>
          <Card.Body>
            <Row>
              <Col>
                <Pie
                  field="repository"
                  history={this.props.history}
                  data={this.props.repos_top_result}
                />
              </Col>
            </Row>
          </Card.Body>
        </Card>
      )
    } else {
      return <LoadingBox />
    }
  }
}

const mapStateToProps = (state) => addMap({}, state.QueryReducer, 'repos_top')

const CReposPie = withRouter(
  connect(mapStateToProps, mapDispatchToProps)(ReposPie)
)

export default CReposPie
