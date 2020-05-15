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

import {
  BaseQueryComponent,
  LoadingBox,
  ErrorBox,
  mapDispatchToProps,
  addMap
} from './common'

class AuthorsHisto extends React.Component {
  prepareDataSet (histos) {
    const createdColor = '135,255,149'
    const reviewedColor = '153,102,102'
    const commentedColor = '169,135,255'
    const eventNameMapping = {
      ChangeCreatedEvent: {
        label: 'Changes authors',
        pointBorderColor: `rgba(${createdColor},1)`,
        pointBackgroundColor: '#fff',
        backgroundColor: `rgba(${createdColor},0.4)`,
        borderColor: `rgba(${createdColor},1)`
      },
      ChangeReviewedEvent: {
        label: 'Reviews authors',
        pointBorderColor: `rgba(${reviewedColor},1)`,
        pointBackgroundColor: '#fff',
        backgroundColor: `rgba(${reviewedColor},0.4)`,
        borderColor: `rgba(${reviewedColor},1)`
      },
      ChangeCommentedEvent: {
        label: 'Comments authors',
        pointBorderColor: `rgba(${commentedColor},1)`,
        pointBackgroundColor: '#fff',
        backgroundColor: `rgba(${commentedColor},0.4)`,
        borderColor: `rgba(${commentedColor},1)`
      }
    }

    const metaData = Object.entries(eventNameMapping)
    const data = {
      labels: histos.ChangeCreatedEvent.buckets.map(x => x.key_as_string),
      datasets: []
    }
    metaData.forEach(desc => {
      data.datasets.push(
        {
          label: desc[1].label,
          data: histos[desc[0]].buckets.map(x => x.doc_count),
          lineTension: 0.5,
          pointBorderColor: desc[1].pointBorderColor,
          pointBackgroundColor: desc[1].pointBackgroundColor,
          backgroundColor: desc[1].backgroundColor,
          borderColor: desc[1].borderColor
        }
      )
    })
    return data
  }

  render () {
    const data = this.prepareDataSet(this.props.data)
    return (
      <Row>
        <Col>
          <Card>
            <Card.Body>
              <Row>
                <Col md={3}>
                  <ListGroup>
                    <ListGroup.Item>Change authors: { this.props.data.ChangeCreatedEvent.total_authors }</ListGroup.Item>
                    <ListGroup.Item>Review authors: { this.props.data.ChangeReviewedEvent.total_authors }</ListGroup.Item>
                    <ListGroup.Item>Comment authors: { this.props.data.ChangeCommentedEvent.total_authors }</ListGroup.Item>
                  </ListGroup>
                </Col>
                <Col md={9}>
                  <Line
                    data={data}
                    width={100}
                    height={30}
                  />
                </Col>
              </Row>
            </Card.Body>
          </Card>
        </Col>
      </Row>
    )
  }
}

AuthorsHisto.propTypes = {
  data: PropTypes.shape({
    ChangeCommentedEvent: PropTypes.object,
    ChangeCreatedEvent: PropTypes.object,
    ChangeReviewedEvent: PropTypes.object
  })
}

class AuthorsHistoStats extends BaseQueryComponent {
  constructor (props) {
    super(props)
    this.state.name = 'authors_histo_stats'
    this.state.graph_type = 'authors_histo_stats'
  }

  render () {
    if (!this.props.authors_histo_stats_loading) {
      if (this.props.authors_histo_stats_error) {
        return <ErrorBox
          error={this.props.authors_histo_stats_error}
        />
      }
      const data = this.props.authors_histo_stats_result
      return (
        <Row>
          <Col>
            <Card>
              <Card.Header>
                <Card.Title>Active authors</Card.Title>
              </Card.Header>
              <Card.Body>
                <AuthorsHisto
                  data={data}
                />
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

const mapStateToProps = state => addMap({}, state.QueryReducer, 'authors_histo_stats')

const CAuthorsHistoStats = withRouter(connect(mapStateToProps, mapDispatchToProps)(AuthorsHistoStats))

export {
  CAuthorsHistoStats
}
