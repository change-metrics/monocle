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

import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import Card from 'react-bootstrap/Card'
import ListGroup from 'react-bootstrap/ListGroup'
import PropTypes from 'prop-types'

import { Line } from 'react-chartjs-2'

import MonoLink from './MLink.bs.js'

import moment from 'moment'

import { hasSmallWidth } from './common'

class ChangeLifeCycleEventsHisto extends React.Component {
  prepareDataSet(histos) {
    const createdColor = '135,255,149'
    const updatedColor = '153,102,102'
    const mergedColor = '169,135,255'
    const abandonedColor = '92,92,92'
    const eventNameMapping = {
      ChangeCreatedEvent: {
        label: 'Created',
        pointBorderColor: 'rgba(' + createdColor + ',1)',
        pointBackgroundColor: '#fff',
        backgroundColor: 'rgba(' + createdColor + ',0.4)',
        borderColor: 'rgba(' + createdColor + ',1)'
      },
      ChangeUpdatedEvent: {
        label: 'Updated',
        pointBorderColor: 'rgba(' + updatedColor + ',1)',
        pointBackgroundColor: '#fff',
        backgroundColor: 'rgba(' + updatedColor + ',0.4)',
        borderColor: 'rgba(' + updatedColor + ',1)'
      },
      ChangeMergedEvent: {
        label: 'Merged',
        pointBorderColor: 'rgba(' + mergedColor + ',1)',
        pointBackgroundColor: '#fff',
        backgroundColor: 'rgba(' + mergedColor + ',0.4)',
        borderColor: 'rgba(' + mergedColor + ',1)'
      },
      ChangeAbandonedEvent: {
        label: 'Abandoned',
        pointBorderColor: 'rgba(' + abandonedColor + ',1)',
        pointBackgroundColor: '#fff',
        backgroundColor: 'rgba(' + abandonedColor + ',0.4)',
        borderColor: 'rgba(' + abandonedColor + ',1)'
      }
    }

    const _histos = Object.entries(histos)
    const data = {
      labels: histos.ChangeCreatedEvent.map((x) => x.date),
      datasets: []
    }
    _histos.forEach((histo) => {
      data.datasets.push({
        label: eventNameMapping[histo[0]].label,
        data: histo[1].map((x) => x.count),
        lineTension: 0.5,
        pointBorderColor: eventNameMapping[histo[0]].pointBorderColor,
        pointBackgroundColor: eventNameMapping[histo[0]].pointBackgroundColor,
        backgroundColor: eventNameMapping[histo[0]].backgroundColor,
        borderColor: eventNameMapping[histo[0]].borderColor
      })
    })
    return data
  }

  render() {
    const data = this.prepareDataSet({
      ChangeCreatedEvent: this.props.created,
      ChangeUpdatedEvent: this.props.updated,
      ChangeMergedEvent: this.props.merged,
      ChangeAbandonedEvent: this.props.abandoned
    })
    return (
      <Row>
        {/* <Col md={{ span: 8, offset: 2 }}> */}
        <Col>
          <Card>
            <Card.Body>
              <Line
                data={data}
                width={100}
                // on small screen the legend takes the whole height so detect and adjust
                height={hasSmallWidth() ? 90 : 68}
                options={{
                  legend: {
                    labels: {
                      boxWidth: 30
                    }
                  }
                }}
              />
            </Card.Body>
          </Card>
        </Col>
      </Row>
    )
  }
}

ChangeLifeCycleEventsHisto.propTypes = {
  created: PropTypes.any,
  updated: PropTypes.any,
  merged: PropTypes.any,
  abandoned: PropTypes.any
}

const ChangesLifeCycleStats = (prop) => (
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
                  {prop.data.created.events_count} changes created by{' '}
                  {prop.data.created.authors_count} authors
                </ListGroup.Item>
                <ListGroup.Item>
                  <MonoLink
                    store={prop.store}
                    filter="state:open"
                    path="changes"
                    name={prop.data.opened + ' opened changes'}
                  />
                </ListGroup.Item>
                <ListGroup.Item>
                  <MonoLink
                    store={prop.store}
                    filter="state:abandoned"
                    path="changes"
                    name={
                      prop.data.abandoned +
                      ' changes abandoned: ' +
                      prop.data.abandoned_ratio +
                      '%'
                    }
                  />
                </ListGroup.Item>
                <ListGroup.Item>
                  <MonoLink
                    store={prop.store}
                    filter="state:merged"
                    path="changes"
                    name={
                      prop.data.merged +
                      ' changes merged: ' +
                      prop.data.merged_ratio +
                      '%'
                    }
                  />
                </ListGroup.Item>
                <ListGroup.Item>
                  <MonoLink
                    store={prop.store}
                    filter="state:self_merged"
                    path="changes"
                    name={
                      prop.data.self_merged +
                      ' changes self merged: ' +
                      prop.data.self_merged_ratio +
                      '%'
                    }
                  />
                </ListGroup.Item>
                <ListGroup.Item>
                  Mean Time To Merge:{' '}
                  {moment.duration(prop.data.ttm_mean, 'seconds').humanize()}
                </ListGroup.Item>
                <ListGroup.Item>
                  Median Deviation of TTM:{' '}
                  {moment
                    .duration(prop.data.ttm_variability, 'seconds')
                    .humanize()}
                </ListGroup.Item>
                <ListGroup.Item>
                  {prop.data.updates_of_changes} updates of changes
                </ListGroup.Item>
                <ListGroup.Item>
                  Changes with tests: {prop.data.changes_with_tests}%
                </ListGroup.Item>
                <ListGroup.Item>
                  {prop.data.iterations_per_change} iterations per change
                </ListGroup.Item>
                <ListGroup.Item>
                  {prop.data.commits_per_change.toFixed(2)} commits per change
                </ListGroup.Item>
              </ListGroup>
            </Col>
            <Col md={8}>
              <ChangeLifeCycleEventsHisto
                created={prop.created_histo}
                updated={prop.updated_histo}
                merged={prop.merged_histo}
                abandoned={prop.abandoned_histo}
              />
            </Col>
          </Row>
        </Card.Body>
      </Card>
    </Col>
  </Row>
)

export default ChangesLifeCycleStats
