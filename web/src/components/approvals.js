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

class ApprovalsPie extends BaseQueryComponent {
  constructor (props) {
    super(props)
    this.state.name = 'approvals_top'
    this.state.graph_type = 'approvals_top'
  }

  render () {
    if (!this.props.approvals_top_loading) {
      if (this.props.approvals_top_error) {
        return <ErrorBox
          error={this.props.approvals_top_error}
        />
      }
      const palette = {
        'Code-Review+2': '#00ff9f',
        'Code-Review+1': '#B6FCD5',
        'Code-Review-1': '#CA5462',
        'Code-Review-2': '#AB0000',
        'Workflow+1': '#00ff9f',
        'Workflow-1': '#AB0000',
        APPROVED: '#00ff9f',
        DISMISSED: '#AB0000',
        COMMENTED: '#B6FCD5',
        CHANGES_REQUESTED: '#CA5462'
      }
      const ignoredApproval = [
        'Code-Review+0',
        'Verified+0',
        'Workflow+0',
        'COMMENTED'
      ]
      return (
        <Row>
          <Col>
            <Card>
              <Card.Header>
                <Card.Title>Approvals</Card.Title>
              </Card.Header>
              <Card.Body>
                <Row>
                  <Col>
                    <Pie
                      data={this.props.approvals_top_result}
                      filtered_items={ignoredApproval}
                      palette={palette}
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

const mapStateToProps = state => addMap({}, state.QueryReducer, 'approvals_top')

const CApprovalsPie = withRouter(connect(mapStateToProps, mapDispatchToProps)(ApprovalsPie))

export {
  CApprovalsPie
}
