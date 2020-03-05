import React from 'react';

import Row from 'react-bootstrap/Row'
import Col from 'react-bootstrap/Col'
import Card from 'react-bootstrap/Card'
import Table from 'react-bootstrap/Table'

import {
    BaseQueryComponent,
    LoadingBox,
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
                      <td>{index}</td>
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
      'most_active_authors_stats',
      'most_active_authors_stats')
  }
  render() {
    if (!this.props.most_active_authors_stats_loading) {
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
      'most_reviewed_authors_stats',
      'most_reviewed_authors_stats')
  }
  render() {
    if (!this.props.most_reviewed_authors_stats_loading) {
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
                      <td>{index}</td>
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
      'peers_exchange_strength',
      'authors_peers_stats')
  }
  render() {
    if (!this.props.authors_peers_stats_loading) {
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

export {
  MostActiveAuthorsStats,
  MostReviewedAuthorsStats,
  AuthorsPeersStats,
}
