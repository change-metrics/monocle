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
import { query } from '../reducers/query'

import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import Card from 'react-bootstrap/Card'

import {
  LoadingBox,
  ErrorBox,
  BaseQueryComponent
} from './common'

import Pie from './pie'

class ApprovalStats extends BaseQueryComponent {
  constructor (props) {
    super(props)
    this.state.name = 'changes_top_approval'
    this.state.graph_type = 'approval_stats'
  }

  render () {
    if (!this.props.approval_stats_loading) {
      if (this.props.approval_stats_error) {
        return <ErrorBox
          error={this.props.approval_stats_error}
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
                <Card.Title>Approvals dispersion stats</Card.Title>
              </Card.Header>
              <Card.Body>
                <Row>
                  <Col>
                    <Pie
                      data={this.props.approval_stats_result}
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

const mapStateToProps = state => {
  return {
    approval_stats_loading: state.QueryReducer.approval_stats_loading,
    approval_stats_result: state.QueryReducer.approval_stats_result,
    approval_stats_error: state.QueryReducer.approval_stats_error
  }
}

const mapDispatchToProps = dispatch => {
  return {
    handleQuery: (params) => dispatch(query(params))
  }
}

const CApprovalStats = connect(mapStateToProps, mapDispatchToProps)(ApprovalStats)

export {
  CApprovalStats
}
