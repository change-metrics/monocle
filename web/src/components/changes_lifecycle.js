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
import ListGroup from 'react-bootstrap/ListGroup'
import PropTypes from 'prop-types'
import { withRouter } from 'react-router-dom'

import { Line } from 'react-chartjs-2'

import moment from 'moment'

import {
  BaseQueryComponent,
  LoadingBox,
  ErrorBox,
  mapDispatchToProps,
  addMap
} from './common'

class ChangeLifeCycleEventsHisto extends React.Component {
  prepareDataSet (histos) {
    const createdColor = '135,255,149'
    const updatedColor = '153,102,102'
    const mergedColor = '169,135,255'
    const abandonedColor = '92,92,92'
    const eventNameMapping = {
      ChangeCreatedEvent: {
        label: 'Created Changes',
        pointBorderColor: `rgba(${createdColor},1)`,
        pointBackgroundColor: '#fff',
        backgroundColor: `rgba(${createdColor},0.4)`,
        borderColor: `rgba(${createdColor},1)`
      },
      ChangeMergedEvent: {
        label: 'Merged Changes',
        pointBorderColor: `rgba(${mergedColor},1)`,
        pointBackgroundColor: '#fff',
        backgroundColor: `rgba(${mergedColor},0.4)`,
        borderColor: `rgba(${mergedColor},1)`
      },
      ChangeAbandonedEvent: {
        label: 'Abandoned Changes',
        pointBorderColor: `rgba(${abandonedColor},1)`,
        pointBackgroundColor: '#fff',
        backgroundColor: `rgba(${abandonedColor},0.4)`,
        borderColor: `rgba(${abandonedColor},1)`
      }
    }

    const metaData = Object.entries(eventNameMapping)
    const data = {
      labels: histos.ChangeCreatedEvent[0].map(x => x.key_as_string),
      datasets: []
    }
    metaData.forEach(desc => {
      data.datasets.push(
        {
          label: desc[1].label,
          data: histos[desc[0]][0].map(x => x.doc_count),
          lineTension: 0.5,
          pointBorderColor: desc[1].pointBorderColor,
          pointBackgroundColor: desc[1].pointBackgroundColor,
          backgroundColor: desc[1].backgroundColor,
          borderColor: desc[1].borderColor
        }
      )
    })
    // merge ChangeCommitForcePushedEvent and ChangeCommitPushedEvent together
    const merged = []
    for (var idx = 0; idx < histos.ChangeCommitForcePushedEvent[0].length; idx++) {
      const d1 = histos.ChangeCommitForcePushedEvent[0][idx]
      const d2 = histos.ChangeCommitPushedEvent[0][idx]
      merged.push(d1.doc_count + d2.doc_count)
    }
    data.datasets.push(
      {
        label: 'Updated Changes',
        data: merged,
        lineTension: 0.5,
        pointBorderColor: `rgba(${updatedColor},1)`,
        pointBackgroundColor: '#fff',
        backgroundColor: `rgba(${updatedColor},0.4)`,
        borderColor: `rgba(${updatedColor},1)`
      }
    )
    return data
  }

  render () {
    const data = this.prepareDataSet(this.props.data)
    return (
      <Row>
        {/* <Col md={{ span: 8, offset: 2 }}> */}
        <Col>
          <Card>
            <Card.Body>
              <Line
                data={data}
                width={100}
                height={68}
              />
            </Card.Body>
          </Card>
        </Col>
      </Row>
    )
  }
}

ChangeLifeCycleEventsHisto.propTypes = {
  data: PropTypes.shape({
    ChangeAbandonedEvent: PropTypes.array,
    ChangeCreatedEvent: PropTypes.array,
    ChangeMergedEvent: PropTypes.array,
    ChangeCommitForcePushedEvent: PropTypes.array
  })
}

class ChangesLifeCycleStats extends BaseQueryComponent {
  constructor (props) {
    super(props)
    this.state.name = 'changes_lifecycle_stats'
    this.state.graph_type = 'changes_lifecycle_stats'
  }

  render () {
    if (!this.props.changes_lifecycle_stats_loading) {
      if (this.props.changes_lifecycle_stats_error) {
        return <ErrorBox
          error={this.props.changes_lifecycle_stats_error}
        />
      }
      const data = this.props.changes_lifecycle_stats_result
      return (
        <Row>
          <Col>
            <Card>
              <Card.Header>
                <Card.Title>Changes lifecycle stats</Card.Title>
              </Card.Header>
              <Card.Body>
                <Row>
                  <Col md={4}>
                    <ListGroup>
                      <ListGroup.Item>
                        {data.ChangeCreatedEvent.events_count} changes created by {data.ChangeCreatedEvent.authors_count} authors
                      </ListGroup.Item>
                      <ListGroup.Item>
                        {data.ChangeCommitForcePushedEvent.events_count + data.ChangeCommitPushedEvent.events_count} updates of changes
                      </ListGroup.Item>
                      <ListGroup.Item>
                        {data.ChangeMergedEvent.events_count} changes merged by {data.ChangeMergedEvent.authors_count} authors
                      </ListGroup.Item>
                      <ListGroup.Item>
                        {data.ChangeAbandonedEvent.events_count} changes abandoned by {data.ChangeAbandonedEvent.authors_count} authors
                      </ListGroup.Item>
                      <ListGroup.Item>
                        Changes abandoned: {data.ratios['abandoned/created']}%
                      </ListGroup.Item>
                      <ListGroup.Item>
                        Changes merged: {data.ratios['merged/created']}%
                      </ListGroup.Item>
                      <ListGroup.Item>
                        {data.ratios['iterations/created']} iterations per change
                      </ListGroup.Item>
                      <ListGroup.Item>
                        Mean time to merge: {moment.duration(data.duration, 'seconds').humanize()}
                      </ListGroup.Item>
                      <ListGroup.Item>
                        {data.commits ? data.commits.toFixed(2) : 'no'} commits per change
                      </ListGroup.Item>
                    </ListGroup>
                  </Col>
                  <Col md={8}>
                    <ChangeLifeCycleEventsHisto
                      data={data.histos}
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

const mapStateToProps = state => addMap({}, state.QueryReducer, 'changes_lifecycle_stats')

const CChangesLifeCycleStats = withRouter(connect(mapStateToProps, mapDispatchToProps)(ChangesLifeCycleStats))

export {
  CChangesLifeCycleStats
}
