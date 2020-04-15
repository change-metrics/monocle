import React from 'react'
import { Link } from 'react-router-dom'

import { connect } from 'react-redux'
import { query } from '../reducers/query'

import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import Card from 'react-bootstrap/Card'
import Table from 'react-bootstrap/Table'
import PropTypes from 'prop-types'

import ConnectionDiagram from './connection_diagram'

import {
  BaseQueryComponent,
  LoadingBox,
  ErrorBox,
  addUrlField
} from './common'

class TopEventsTable extends React.Component {
  rowStyleFormat (average, value) {
    if (value >= average) {
      return { color: 'green' }
    }
  }

  render () {
    if (!this.props.data || !this.props.data.items) {
      return <ErrorBox error={{ status: 0, data: 'Invalid data' }}/>
    }
    return (
      <Row>
        <Col>
          <Card>
            <Card.Header>
              <Card.Title>{this.props.title}</Card.Title>
            </Card.Header>
            <Card.Body>
              <Table striped responsive bordered hover>
                <thead>
                  <tr>
                    <th>#</th>
                    <th>ID</th>
                    <th>count</th>
                  </tr>
                </thead>
                <tbody>
                  {this.props.data.items.map((x, index) =>
                    <tr key={index}>
                      <td style={this.rowStyleFormat(this.props.data.count_avg, x.doc_count)}>{index + 1}</td>
                      <td><Link to={addUrlField('authors', x.key)}>{x.key}</Link></td>
                      <td>{x.doc_count}</td>
                    </tr>)}
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
  constructor (props) {
    super(props)
    this.state.name = 'most_active_authors_stats'
    this.state.graph_type = 'most_active_authors_stats'
  }

  render () {
    if (!this.props.most_active_authors_stats_loading) {
      if (this.props.most_active_authors_stats_error) {
        return <ErrorBox
          error={this.props.most_active_authors_stats_error}
        />
      }
      const data = this.props.most_active_authors_stats_result
      return (
        <Row>
          <Col>
            <Card>
              <Card.Header>
                <Card.Title>Most active authors stats</Card.Title>
              </Card.Header>
              <Card.Body>
                <Row>
                  <Col>
                    <TopEventsTable
                      data={data.ChangeCreatedEvent}
                      title="By Created Changes"
                    />
                  </Col>
                  <Col>
                    <TopEventsTable
                      data={data.ChangeMergedEvent}
                      title="By Merged Changes"
                    />
                  </Col>
                </Row>
                <Row><Col><p></p></Col></Row>
                <Row>
                  <Col>
                    <TopEventsTable
                      data={data.ChangeReviewedEvent}
                      title="By Reviewed Changes"
                    />
                  </Col>
                  <Col>
                    <TopEventsTable
                      data={data.ChangeCommentedEvent}
                      title="By Commented Changes"
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
  constructor (props) {
    super(props)
    this.state.name = 'most_reviewed_authors_stats'
    this.state.graph_type = 'most_reviewed_authors_stats'
  }

  render () {
    if (!this.props.most_reviewed_authors_stats_loading) {
      if (this.props.most_reviewed_authors_stats_error) {
        return <ErrorBox
          error={this.props.most_reviewed_authors_stats_error}
        />
      }
      const data = this.props.most_reviewed_authors_stats_result
      return (
        <Row>
          <Col>
            <Card>
              <Card.Header>
                <Card.Title>Most reviewed authors stats</Card.Title>
              </Card.Header>
              <Card.Body>
                <Row>
                  <Col>
                    <TopEventsTable
                      data={data.reviewed}
                      title="Reviews"
                    />
                  </Col>
                  <Col>
                    <TopEventsTable
                      data={data.commented}
                      title="Comments"
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

class TopStrengthsTable extends React.Component {
  render () {
    return (
      <Row>
        <Col>
          <Card>
            <Card.Header>
              <Card.Title>{this.props.title}</Card.Title>
            </Card.Header>
            <Card.Body>
              <Row>
                <Col>
                  <ConnectionDiagram data={this.props.data}/>
                </Col>
              </Row>
              <Row>
                <Col>
                  <Table striped responsive bordered hover>
                    <thead>
                      <tr>
                        <th>#</th>
                        <th>Peers</th>
                        <th>Strength</th>
                      </tr>
                    </thead>
                    <tbody>
                      {this.props.data.slice(0, 15).map((x, index) =>
                        <tr key={index}>
                          <td>{index + 1}</td>
                          <td><Link to={addUrlField('authors', x[0][0] + ',' + x[0][1])}>{x[0][0]} and {x[0][1]}</Link></td>
                          <td>{x[1]}</td>
                        </tr>)}
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
  constructor (props) {
    super(props)
    this.state.name = 'peers_exchange_strength'
    this.state.graph_type = 'authors_peers_stats'
  }

  render () {
    if (!this.props.authors_peers_stats_loading) {
      if (this.props.authors_peers_stats_error) {
        return <ErrorBox
          error={this.props.authors_peers_stats_error}
        />
      }
      const data = this.props.authors_peers_stats_result
      return (
        <TopStrengthsTable
          data={data}
          title="Peers strength"
        />
      )
    } else {
      return <LoadingBox />
    }
  }
}

const mapStateToProps = state => {
  return {
    most_active_authors_stats_loading: state.QueryReducer.most_active_authors_stats_loading,
    most_active_authors_stats_result: state.QueryReducer.most_active_authors_stats_result,
    most_active_authors_stats_error: state.QueryReducer.most_active_authors_stats_error,
    most_reviewed_authors_stats_loading: state.QueryReducer.most_reviewed_authors_stats_loading,
    most_reviewed_authors_stats_result: state.QueryReducer.most_reviewed_authors_stats_result,
    most_reviewed_authors_stats_error: state.QueryReducer.most_reviewed_authors_stats_error,
    authors_peers_stats_loading: state.QueryReducer.authors_peers_stats_loading,
    authors_peers_stats_result: state.QueryReducer.authors_peers_stats_result,
    authors_peers_stats_error: state.QueryReducer.authors_peers_stats_error
  }
}

const mapDispatchToProps = dispatch => {
  return {
    handleQuery: (params) => dispatch(query(params))
  }
}

const CMostActiveAuthorsStats = connect(mapStateToProps, mapDispatchToProps)(MostActiveAuthorsStats)
const CMostReviewedAuthorsStats = connect(mapStateToProps, mapDispatchToProps)(MostReviewedAuthorsStats)
const CAuthorsPeersStats = connect(mapStateToProps, mapDispatchToProps)(AuthorsPeersStats)

export {
  CMostActiveAuthorsStats,
  CMostReviewedAuthorsStats,
  CAuthorsPeersStats
}
