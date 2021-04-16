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
import Table from 'react-bootstrap/Table'
import PropTypes from 'prop-types'
import { withRouter, Link } from 'react-router-dom'

import ConnectionDiagram from './connection_diagram'

import {
  BaseQueryComponent,
  LoadingBox,
  ErrorBox,
  addUrlField,
  mapDispatchToProps,
  addMap
} from './common'

class TopEventsTable extends React.Component {
  rowStyleFormat(average, value) {
    if (value >= average) {
      return { color: 'green' }
    }
  }

  render() {
    if (!this.props.data || !this.props.data.items) {
      return <ErrorBox error={{ status: 0, data: 'Invalid data' }} />
    }
    return (
      <Row>
        <Col>
          <Card className="rounded border">
            <Card.Header className="bg-white text-center">
              <Card.Title>{this.props.title}</Card.Title>
            </Card.Header>
            <Card.Body>
              <Table striped responsive bordered hover size="sm">
                <thead>
                  <tr>
                    <th>#</th>
                    <th>ID</th>
                    <th>count</th>
                  </tr>
                </thead>
                <tbody>
                  {this.props.data.items.map((x, index) => (
                    <tr key={index}>
                      <td
                        style={this.rowStyleFormat(
                          this.props.data.count_avg,
                          x.doc_count
                        )}
                      >
                        {index + 1}
                      </td>
                      <td>
                        <Link to={addUrlField('authors', x.key)}>{x.key}</Link>
                      </td>
                      <td>{x.doc_count}</td>
                    </tr>
                  ))}
                </tbody>
              </Table>
            </Card.Body>
          </Card>
        </Col>
      </Row>
    )
  }
}

TopEventsTable.propTypes = {
  title: PropTypes.string.isRequired,
  data: PropTypes.shape({
    items: PropTypes.array,
    count_avg: PropTypes.number
  })
}

class MostActiveAuthorsStats extends BaseQueryComponent {
  constructor(props) {
    super(props)
    this.state.name = 'most_active_authors_stats'
    this.state.graph_type = 'most_active_authors_stats'
  }

  render() {
    if (!this.props.most_active_authors_stats_loading) {
      if (this.props.most_active_authors_stats_error) {
        return <ErrorBox error={this.props.most_active_authors_stats_error} />
      }
      const data = this.props.most_active_authors_stats_result
      return (
        <Row>
          <Col>
            <Card className="rounded border-0 border-top">
              <Card.Header className="bg-white text-center">
                <Card.Title>Most active authors stats</Card.Title>
              </Card.Header>
              <Card.Body>
                <Row>
                  <Col md>
                    <TopEventsTable
                      data={data.ChangeCreatedEvent}
                      title="By created changes"
                    />
                  </Col>
                  <Col md>
                    <TopEventsTable
                      data={data.ChangeMergedEvent}
                      title="By merged changes"
                    />
                  </Col>
                </Row>
                <Row>
                  <Col>
                    <p></p>
                  </Col>
                </Row>
                <Row>
                  <Col md>
                    <TopEventsTable
                      data={data.ChangeReviewedEvent}
                      title="By reviewed changes"
                    />
                  </Col>
                  <Col md>
                    <TopEventsTable
                      data={data.ChangeCommentedEvent}
                      title="By commented changes"
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

class MostReviewedAuthorsStats extends BaseQueryComponent {
  constructor(props) {
    super(props)
    this.state.name = 'most_reviewed_authors_stats'
    this.state.graph_type = 'most_reviewed_authors_stats'
  }

  render() {
    if (!this.props.most_reviewed_authors_stats_loading) {
      if (this.props.most_reviewed_authors_stats_error) {
        return <ErrorBox error={this.props.most_reviewed_authors_stats_error} />
      }
      const data = this.props.most_reviewed_authors_stats_result
      return (
        <Row>
          <Col>
            <Card className="rounded border-0 border-top">
              <Card.Header className="bg-white text-center">
                <Card.Title>Most reviewed authors stats</Card.Title>
              </Card.Header>
              <Card.Body>
                <Row>
                  <Col md>
                    <TopEventsTable data={data.reviewed} title="Reviews" />
                  </Col>
                  <Col md>
                    <TopEventsTable data={data.commented} title="Comments" />
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

class NewContributorsStats extends BaseQueryComponent {
  constructor(props) {
    super(props)
    this.state.name = 'new_contributors'
    this.state.graph_type = 'new_contributors'
  }

  render() {
    if (!this.props.new_contributors_loading) {
      if (this.props.new_contributors_error) {
        return <ErrorBox error={this.props.new_contributors_error} />
      }
      const data = this.props.new_contributors_result
      return (
        <Row>
          <Col>
            <Card className="rounded border-0 border-top">
              <Card.Header className="bg-white text-center">
                <Card.Title>New contributors stats</Card.Title>
              </Card.Header>
              <Card.Body>
                <Row>
                  <Col>
                    <TopEventsTable data={data} title="Active Authors" />
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

class TopStrengthsTable extends React.Component {
  render() {
    return (
      <Row>
        <Col>
          <Card className="rounded border-0 border-top">
            <Card.Header className="bg-white text-center">
              <Card.Title>{this.props.title}</Card.Title>
            </Card.Header>
            <Card.Body>
              <Row>
                <Col>
                  <ConnectionDiagram data={this.props.data} />
                </Col>
              </Row>
              <Row>
                <Col>
                  <Table striped responsive bordered hover size="sm">
                    <thead>
                      <tr>
                        <th>#</th>
                        <th>Peers</th>
                        <th>Strength</th>
                      </tr>
                    </thead>
                    <tbody>
                      {this.props.data.slice(0, 15).map((x, index) => (
                        <tr key={index}>
                          <td>{index + 1}</td>
                          <td>
                            <Link
                              to={addUrlField(
                                'authors',
                                x[0][0] + ',' + x[0][1]
                              )}
                            >
                              {x[0][0]} and {x[0][1]}
                            </Link>
                          </td>
                          <td>{x[1]}</td>
                        </tr>
                      ))}
                    </tbody>
                  </Table>
                </Col>
              </Row>
            </Card.Body>
          </Card>
        </Col>
      </Row>
    )
  }
}

TopStrengthsTable.propTypes = {
  title: PropTypes.string.isRequired,
  data: PropTypes.array.isRequired
}

class AuthorsPeersStats extends BaseQueryComponent {
  constructor(props) {
    super(props)
    this.state.name = 'peers_exchange_strength'
    this.state.graph_type = 'authors_peers_stats'
  }

  render() {
    if (!this.props.authors_peers_stats_loading) {
      if (this.props.authors_peers_stats_error) {
        return <ErrorBox error={this.props.authors_peers_stats_error} />
      }
      const data = this.props.authors_peers_stats_result
      return <TopStrengthsTable data={data} title="Peers strength" />
    } else {
      return <LoadingBox />
    }
  }
}

const mapStateToProps = (state) => {
  const map = {}

  addMap(map, state.QueryReducer, 'new_contributors')
  addMap(map, state.QueryReducer, 'most_active_authors_stats')
  addMap(map, state.QueryReducer, 'most_reviewed_authors_stats')
  addMap(map, state.QueryReducer, 'authors_peers_stats')

  return map
}

const CMostActiveAuthorsStats = withRouter(
  connect(mapStateToProps, mapDispatchToProps)(MostActiveAuthorsStats)
)
const CNewContributorsStats = withRouter(
  connect(mapStateToProps, mapDispatchToProps)(NewContributorsStats)
)
const CMostReviewedAuthorsStats = withRouter(
  connect(mapStateToProps, mapDispatchToProps)(MostReviewedAuthorsStats)
)
const CAuthorsPeersStats = withRouter(
  connect(mapStateToProps, mapDispatchToProps)(AuthorsPeersStats)
)

export {
  CNewContributorsStats,
  CMostActiveAuthorsStats,
  CMostReviewedAuthorsStats,
  CAuthorsPeersStats
}
