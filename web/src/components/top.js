import React from 'react';

import { connect } from 'react-redux';
import { query } from '../reducers/query'

import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import Card from 'react-bootstrap/Card'
import Table from 'react-bootstrap/Table'

import {
    BaseQueryComponent,
    LoadingBox,
    ErrorBox,
} from './common'

class TopEventsTable extends React.Component {
  render() {
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
                  {this.props.data.map((x, index) =>
                    <tr key={index}>
                      <td>{index + 1}</td>
                      <td>{x.key}</td>
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

class MostActiveAuthorsStats extends BaseQueryComponent {
  componentDidUpdate(prevProps) {
    this.queryBackend(
      prevProps,
      this.props.index,
      'most_active_authors_stats',
      'most_active_authors_stats',
      this.props.handleQuery)
  }
  render() {
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
                      data={data.ChangeCreatedEvent.tops}
                      title="Changes"
                    />
                  </Col>
                  <Col>
                    <TopEventsTable
                      data={data.ChangeCommentedEvent.tops}
                      title="Comments"
                    />
                  </Col>
                  <Col>
                    <TopEventsTable
                      data={data.ChangeReviewedEvent.tops}
                      title="Reviews"
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
  componentDidUpdate(prevProps) {
    this.queryBackend(
      prevProps,
      this.props.index,
      'most_reviewed_authors_stats',
      'most_reviewed_authors_stats',
      this.props.handleQuery)
  }
  render() {
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
                      data={data.reviewed.tops}
                      title="Reviews"
                    />
                  </Col>
                  <Col>
                    <TopEventsTable
                      data={data.commented.tops}
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
  render() {
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
                    <th>Peers</th>
                    <th>Strength</th>
                  </tr>
                </thead>
                <tbody>
                  {this.props.data.map((x, index) =>
                    <tr key={index}>
                      <td>{index + 1}</td>
                      <td>{x[0][0]} and {x[0][1]}</td>
                      <td>{x[1]}</td>
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


class AuthorsPeersStats extends BaseQueryComponent {
  componentDidUpdate(prevProps) {
    this.queryBackend(
      prevProps,
      this.props.index,
      'peers_exchange_strength',
      'authors_peers_stats',
      this.props.handleQuery)
  }
  render() {
    if (!this.props.authors_peers_stats_loading) {
      if (this.props.authors_peers_stats_error) {
        return <ErrorBox
          error={this.props.authors_peers_stats_error}
        />
      }
      const data = this.props.authors_peers_stats_result
      return (
        <TopStrengthsTable
          data={data.slice(0, 10)}
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
    filter_loaded_from_url: state.FiltersReducer.filter_loaded_from_url,
    filter_gte: state.FiltersReducer.filter_gte,
    filter_lte: state.FiltersReducer.filter_lte,
    filter_repository: state.FiltersReducer.filter_repository,
    filter_index: state.FiltersReducer.filter_index,
    filter_interval: state.FiltersReducer.filter_interval,
    filter_exclude_authors: state.FiltersReducer.filter_exclude_authors,
    filter_authors: state.FiltersReducer.filter_authors,
    most_active_authors_stats_loading: state.QueryReducer.most_active_authors_stats_loading,
    most_active_authors_stats_result: state.QueryReducer.most_active_authors_stats_result,
    most_active_authors_stats_error: state.QueryReducer.most_active_authors_stats_error,
    most_reviewed_authors_stats_loading: state.QueryReducer.most_reviewed_authors_stats_loading,
    most_reviewed_authors_stats_result: state.QueryReducer.most_reviewed_authors_stats_result,
    most_reviewed_authors_stats_error: state.QueryReducer.most_reviewed_authors_stats_error,
    authors_peers_stats_loading: state.QueryReducer.authors_peers_stats_loading,
    authors_peers_stats_result: state.QueryReducer.authors_peers_stats_result,
    authors_peers_stats_error: state.QueryReducer.authors_peers_stats_error,
  }
}

const mapDispatchToProps = dispatch => {
  return {
    handleQuery: (params) => dispatch(query(params)),
  }
}

const CMostActiveAuthorsStats = connect(mapStateToProps, mapDispatchToProps)(MostActiveAuthorsStats);
const CMostReviewedAuthorsStats = connect(mapStateToProps, mapDispatchToProps)(MostReviewedAuthorsStats);
const CAuthorsPeersStats = connect(mapStateToProps, mapDispatchToProps)(AuthorsPeersStats);

export {
  CMostActiveAuthorsStats,
  CMostReviewedAuthorsStats,
  CAuthorsPeersStats,
}
